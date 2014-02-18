/*
* Copyright 2009, 2010 Free Software Foundation, Inc.
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

#include <string.h>
#include <cstdio>
#include <fstream>

#include "Configuration.h"
#include "Sockets.h"
#include "Logger.h"
#include "Timeval.h"


using namespace std;

// Reference to a global config table, used all over the system.
extern ConfigurationTable gConfig;


/** The global logging lock. */
static Mutex gLogLock;



/**@ The global alarms table. */
//@{
Mutex           alarmsLock;
list<string>    alarmsList;
void            addAlarm(const string&);
//@}






/** Names of the logging levels. */
const char* levelNames[] =
	{ "FORCE", "ERROR", "ALARM", "WARN", "NOTICE", "INFO", "DEBUG", "DEEPDEBUG" };
const unsigned numLevels = 8;

ostream& operator<<(ostream& os, Log::Level level)
{
	unsigned il = (unsigned)level;
	assert(il<numLevels);
	os << levelNames[il];
	return os;
}


/** Given a string, return the corresponding level name. */
Log::Level gLookupLevel(const char* name)
{
	for (unsigned i=0; i<numLevels; i++) {
		if (strcmp(levelNames[i],name)==0) return (Log::Level)i;
	}
	// This should never be called with a bogus name.
	LOG(ERROR) << "undefined logging level " << name << "defaulting to FORCE";
	return Log::LOG_FORCE;
}

/** Return the current logging level for a given source file. */
Log::Level gLoggingLevel(const char* filename)
{
	const string keyName = string("Log.Level.") + string(filename);
	if (gConfig.defines(keyName)) return gLookupLevel(gConfig.getStr(keyName));
	return gLookupLevel(gConfig.getStr("Log.Level"));
}





/** The current global log sink. */
static FILE *gLoggingFile = stdout;

void gSetLogFile(FILE *wFile)
{
	gLogLock.lock();
	gLoggingFile = wFile;
	gLogLock.unlock();
}


bool gSetLogFile(const char *name)
{
	assert(name);
	LOG(FORCE) << "setting log path to " << name;
	bool retVal = true;
	gLogLock.lock();
	FILE* newLoggingFile = fopen(name,"a+");
	if (!newLoggingFile) {
		LOG(ERROR) << "cannot open \"" << name << "\" for logging.";
		retVal = false;
	} else {
		gLoggingFile = newLoggingFile;
	}
	gLogLock.unlock();
	LOG(FORCE) << "new log path " << name;
	return retVal;
}


// copies the alarm list and returns it. list supposed to be small.
std::list<std::string> gGetLoggerAlarms()
{
    alarmsLock.lock();
    std::list<std::string> ret;
    // excuse the "complexity", but to use std::copy with a list you need
    // an insert_iterator - copy technically overwrites, doesn't insert.
    std::insert_iterator< std::list<std::string> > ii(ret, ret.begin());
    std::copy(alarmsList.begin(), alarmsList.end(), ii);
    alarmsLock.unlock();
    return ret;
}

// Add an alarm to the alarm list, and send it out via udp
//
// On the first call we read the ip and port from the configuration
// TODO - is there any global setup function where this should be done? -- Alon
void addAlarm(const string& s)
{
	// Socket open and close on every alarm - wise?
	// Probably.  That way we are sure to pick up changes in the target address.
	// Alarms should not happen often.
	if (gConfig.defines("Log.Alarms.TargetIP")) {
		UDPSocket alarmsocket(0,
			gConfig.getStr("Log.Alarms.TargetIP"),
			gConfig.getNum("Log.Alarms.TargetPort"));
		alarmsocket.write(s.c_str());
	}
    // append to list and reduce list to max alarm count
    alarmsLock.lock();
    alarmsList.push_back(s);
	unsigned maxAlarms = gConfig.getNum("Log.Alarms.Max");
    while (alarmsList.size() > maxAlarms) alarmsList.pop_front();
    alarmsLock.unlock();
}


Log::~Log()
{
	// XXX always handle alarms, even if the logging level is too low
	if (mReportLevel == LOG_ALARM) {
		addAlarm(mStream.str().c_str());
		std::cerr << mStream.str() << std::endl;
	}
	// Current logging level was already checked by the macro.
	// So just log.
	gLogLock.lock();
	mStream << std::endl;
	fprintf(gLoggingFile, "%s", mStream.str().c_str());
	fflush(gLoggingFile);
	gLogLock.unlock();
}


ostringstream& Log::get()
{
	Timeval now;
	mStream.precision(4);
	mStream << now << ' ' << mReportLevel <<  ' ';
	return mStream;
}



void gLogInit(const char* defaultLevel)
{
	// Define defaults in the global config
	if (!gConfig.defines("Log.Level")) {
		gConfig.set("Log.Level",defaultLevel);
		LOG(FORCE) << "Setting initial global logging level to " << defaultLevel;
	}
	if (!gConfig.defines("Log.Alarms.TargetPort")) {
		gConfig.set("Log.Alarms.TargetPort",DEFAULT_ALARM_PORT);
	}
	if (!gConfig.defines("Log.Alarms.Max")) {
		gConfig.set("Log.Alarms.Max",DEFAULT_MAX_ALARMS);
	}
}




// vim: ts=4 sw=4
