/*
* Copyright 2009 Kestrel Signal Processing, Inc.
*
* This software is distributed under the terms of the GNU Affero Public License.
* See the COPYING file in the main directory for details.
*
* This use of this software may be subject to additional restrictions.
* See the LEGAL file in the main directory for details.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "HLR.h"
#include <Logger.h>
#include <Threads.h>
#include <stdio.h>
#include <string.h>
#include <map>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

using namespace std;


/**
	Not all systems include getline, so we provide our own.
	@param fp A FILE* to read
	@return a C string to be freed by the caller.
*/
char *getline(FILE *fp)
{
	size_t bSize = 200;
	unsigned i = 0;
	char *buf = (char*)malloc(bSize);
	while (true) {
		char c = getc(fp);
		// EOF?
		if (feof(fp)) {
			free(buf);
			return NULL;
		}
		// EOL?
		if (c=='\n') {
			buf[i]='\0';
			LOG(DEBUG) << "getline got: " << buf;
			return buf;
		}
		buf[i++] = c;
		// resize buffer?
		if (i==bSize) {
			bSize *= 2;
			buf=(char*)realloc(buf,bSize);
			if (!buf) {
				LOG(ERROR) << "line too big for memory";
				buf[i]='\0';
				return buf;
			}
		}
	}
	// not reached
	return NULL;
}

const HLR::Status HLR::CombinedStatus[4][4] = {
	{SUCCESS,FAILURE,DELAYED,TRYAGAIN},
	{FAILURE,FAILURE,FAILURE,FAILURE},
	{DELAYED,DELAYED,DELAYED,DELAYED},
	{TRYAGAIN,TRYAGAIN,TRYAGAIN,TRYAGAIN}
};



char *AsteriskHLR::getAsteriskLine(const char* command, const char* tag)
{
	LOG(DEBUG) << "getAsteriskLine cmd=\"" << command << "\" tag=\"" << tag << "\"";
	char fullCommand[100];
	if (strlen(command)>100) {
		LOG(ERROR) << "Asterisk command too long";
		return NULL;
	}
	sprintf(fullCommand,"asterisk -rx \"%s\"", command);
	FILE* asterisk = popen(fullCommand,"r");
	if (!asterisk) {
		LOG(ALARM) << "Asterisk call \"" << command << "\" failed";
		return NULL;
	}
	while (!feof(asterisk)) {
		char * line = getline(asterisk);
		if (!line) break;
		if (strstr(line,tag)) {
			pclose(asterisk);
			LOG(DEBUG) << "getAsteriskLine got: " << line;
			return line;
		}
		free(line);
	}
	pclose(asterisk);
	return NULL;
}



char *AsteriskHLR::getIMSI(const char *ISDN)
{
	if (!ISDN) {
		LOG(WARN) << "AsteriskHLR::getIMSI attempting lookup of NULL IMSI";
		return NULL;
	}
	reloadConfig();
	char *cached = mIMSICache.read(ISDN);
	if (cached) return cached;

	char command[100];
	sprintf(command,"dialplan show %s@sip-local", ISDN);
	char *line = getAsteriskLine(command,ISDN);
	// No line means the ISDN isn't resolvable in the dialplan.
	if (!line) return NULL;
	char *SIP = strstr(line,"SIP");
	char *digits;
	char *IMSI;
	if (!SIP) goto badParse;
	// A valid entry is 22-23 digits, and maybe some trailing chars.
	if (strlen(SIP)<22) goto badParse;
	// Skip "SIP".
	digits = SIP+4;
	// You can't just initialize this after the goto,
	// at least on some compliers.
	char *endp;
	endp = strchr(digits,')');
	if (!endp) goto badParse;
	*endp = '\0';
	IMSI = strdup(digits);
	free(line);
	mIMSICache.write(ISDN,IMSI);
	return IMSI;

badParse:
	// Getting here isn't always an error.
	// It may just mean the number isn't in the registry.
	LOG(DEBUG) << "AsteriskHLR cannot parse " << line;
	free(line);
	return NULL;
}


char *AsteriskHLR::getCLIDLocal(const char* IMSI)
{
	reloadConfig();
	char *cached = mCLIDLocalCache.read(IMSI);
	if (cached) return cached;

	char command[100];
	sprintf(command,"sip show user %s", IMSI);
	char *line = getAsteriskLine(command,"Callerid");
	// No line means the IMSI is not provisioned.
	if (!line) return NULL;
	// Isolate the string we need.
	char *numberStart;
	char *numberEnd;
	char *CLID;
	numberStart = strchr(line,'<');
	if (!numberStart) goto badParse;
	numberEnd = strchr(numberStart,'>');
	if (!numberEnd) goto badParse;
	*numberEnd = '\0';
	CLID = strdup(numberStart+1);
	free(line);
	mCLIDLocalCache.write(IMSI,CLID);
	return CLID;

	// Handle errors here.
badParse:
	LOG(ERROR) << "AsteriskHLR::getCLIDLocal cannot parse " << line;
	free(line);
	return NULL;

}



char *AsteriskHLR::getCLIDGlobal(const char* IMSI)
{
	reloadConfig();
	char *cached = mCLIDGlobalCache.read(IMSI);
	if (cached) return cached;

	// FIXME
	LOG(WARN) << "AsteriskHLR::getCLIDGlobal need to implement!!";
	return strdup("883510001250000");
}



char *AsteriskHLR::mapCLIDGlobal(const char *local)
{
	char *IMSI = getIMSI(local);
	if (!IMSI) return NULL;
	char *global = getCLIDGlobal(IMSI);
	free(IMSI);
	return global;
}



char *AsteriskHLR::getRegistrationIP(const char* IMSI)
{
	char *cached = mRegistrationCache.read(IMSI);
	if (cached) return cached;

	char command[100];
	sprintf(command,"database showkey SIP/Registry/%s", IMSI);
	char *line = getAsteriskLine(command,IMSI);
	// No line means the user isn't registered.
	if (!line) return NULL;

	// Find the first three colons
	char *first;
	char *second;
	char *third;
	char *IPString;
	// : #1, start of IP #
	first = strchr(line,':');
	if (!first) goto badParse;
	first++;
	// Skip leading spaces
	while (isspace(*first)) first++;
	// : #2, start of port #
	second = strchr(first+1,':');
	if (!second) goto badParse;
	// : #3, end of port #
	third = strchr(second+1,':');
	if (!third) goto badParse;

	// Get the string we need
	*third = '\0';
	IPString = strdup(first);
	free(line);
	mRegistrationCache.write(IMSI,IPString,20);
	return IPString;

	// Handle errors here.
badParse:
	LOG(ERROR) << "AsteriskHLR::getRegistrationIP cannot parse " << line;
	free(line);
	return NULL;
}



HLR::Status AsteriskHLR::addUser(const char* IMSI, const char* CLID)
{
	if (lockedConfig()) return TRYAGAIN;
	// Open the config file.
	static const char filename[] = "/etc/asterisk/sip.conf";
	FILE *cf = fopen(filename,"a");
	if (!cf) {
		LOG(ALARM) << "AsteriskHLR::asddUser cannot open " << filename;
		return HLR::FAILURE;
	}
	// The standard user template.
	// FIXME -- This should somehow be configurable in real time.
	time_t now = time(NULL);
	fprintf(cf,";provisioned %s",ctime(&now));
	fprintf(cf,"[%s]\n",IMSI);
	fprintf(cf,"callerid=%s\n",CLID);
	fprintf(cf,"canreinvite=no\ntype=friend\ncontext=sip-local\nallow=gsm\nhost=dynamic\ndtmfmode=info\n\n");
	// All done.
	fclose(cf);
	// Reload the config.
	mNeedSIPReload = true;
	HLR::Status stat1 = reloadSIP();
	// Go ahead and add the extension now, too.
	HLR::Status stat2 = addAddress(IMSI,CLID);

	return CombinedStatus[stat1][stat2];
}


HLR::Status AsteriskHLR::addAddress(const char* IMSI, const char* address)
{
	if (lockedConfig()) return TRYAGAIN;

	// extensions.conf needs to #include "extensions.local.conf"

	// Open the config file.
	static const char filename[] = "/etc/asterisk/extensions.local.conf";
	FILE *cf = fopen(filename,"a");
	if (!cf) {
		LOG(ALARM) << "AsteriskHLR::asddAddress cannot open " << filename;
		return FAILURE;
	}
	time_t now = time(NULL);
	fprintf(cf,";provisioned %s",ctime(&now));
	fprintf(cf,"exten => %s,1,Dial(SIP/%s)\n", address, IMSI);
	fclose(cf);
	// Reload the config.
	mNeedDialplanReload = true;
	return reloadDialplan();
}



HLR::Status AsteriskHLR::reloadSIP()
{
	LOG(DEBUG) << "AsteriskHLR::reloadSIP needReload=" << mNeedSIPReload << " elapsed=" << mLastSIPReloadTime.elapsed();
	if (!mNeedSIPReload) return SUCCESS;
	if (mLastSIPReloadTime.elapsed()<1000*mHoldoffTime) return DELAYED;
	if (system("asterisk -rx \"sip reload\"")) {
		LOG(ALARM) << "AsteriskHLR::reloadSIP Asterisk call failed";
		return FAILURE;
	}
	mLastSIPReloadTime.now();
	mNeedSIPReload = false;
	return SUCCESS;
}


HLR::Status AsteriskHLR::reloadDialplan()
{
	LOG(DEBUG) << "AsteriskHLR::reloadDialplan needReload=" << mNeedDialplanReload << " elapsed=" << mLastDialplanReloadTime.elapsed();
	if (!mNeedDialplanReload) return SUCCESS;
	if (mLastDialplanReloadTime.elapsed()<1000*mHoldoffTime) return DELAYED;
	if (system("asterisk -rx \"dialplan reload\"")) {
		LOG(ALARM) << "AsteriskHLR::reloadDialplan Asterisk call failed";
		return FAILURE;
	}
	mLastDialplanReloadTime.now();
	mNeedDialplanReload = false;
	return SUCCESS;
}


HLR::Status AsteriskHLR::reloadConfig()
{
	return CombinedStatus[reloadSIP()][reloadDialplan()];
}




void HLRCache::write(const char* key, const char* value, unsigned lifetime)
{
	mLock.lock();
	if (mTable.size()>MaxSize) flush();
	mTable[key] = HLRCacheEntry(value,lifetime);
	mLock.unlock();
}


char* HLRCache::read(const char* key)
{
	if (!key) return NULL;
	char *retVal = NULL;
	mLock.lock();
	map<string,HLRCacheEntry>::iterator itr = mTable.find(key);
	if (itr==mTable.end()) {
		mLock.unlock();
		return NULL;
	}
	if (itr!=mTable.end()) retVal = strdup(itr->second.value());
	if (itr->second.expired()) {
		mTable.erase(itr);
		mLock.unlock();
		return NULL;
	}
	mLock.unlock();
	return retVal;
}


void HLRCache::flush()
{
	// Discard expired entries.
	// FIXME -- Implement this.
}



bool AsteriskHLR::lockedConfig()
{
	// Return true if the lockfile exists.
	struct stat dummy;
	int s = stat("/etc/asterisk/HLR.lock",&dummy);
	LOG(DEBUG) << "AsteriskHLR::lockedConfig stat=" << s;
	return s==0;
}


bool AsteriskHLR::useGateway(const char* ISDN)
{
	// FIXME -- Do something more general in Asterisk.
	// This is a hack for Burning Man.
	int cmp = strncmp(ISDN,"88351000125",11);
    return cmp!=0;
}






// vim: ts=4 sw=4
