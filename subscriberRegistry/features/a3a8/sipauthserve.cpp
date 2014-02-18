/*
* Copyright 2011 Kestrel Signal Processing, Inc.
* Copyright 2011 Range Networks, Inc.
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


#include <arpa/inet.h>
#include <cstdlib>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <fstream>
#include <iostream>
#include <netinet/in.h>
#include <osip2/osip.h>
#include <osipparser2/osip_message.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <Logger.h>
#include <Configuration.h>
#include "servershare.h"
#include "SubscriberRegistry.h"

using namespace std;

ConfigurationTable gConfig("/etc/OpenBTS/OpenBTS.db");
Log dummy("sipauthserve", gConfig.getStr("Log.Level").c_str(), LOG_LOCAL7);

int my_udp_port;

// just using this for the database access
SubscriberRegistry gSubscriberRegistry;

void prettyPrint(const char *label, osip_message_t *sip)
{
	char *dest=NULL;
	size_t length=0;
	int i = osip_message_to_str(sip, &dest, &length);
	if (i!=0) {
		LOG(ERR) << "cannot get printable message";
	} else {
		LOG(INFO) << label << ":\n" << dest;
		osip_free(dest);
	}
}

string imsiFromSip(osip_message_t *sip)
{
	int i;
	char *dest;
	osip_uri_t *fromUri = osip_from_get_url(sip->from);
	if (!fromUri) {
		LOG(ERR) << "osip_from_get_url problem";
		return "";
	}
	i = osip_uri_to_str(fromUri, &dest);
	string imsi = dest;
	osip_free(dest);
	return imsi;
}

string imsiToSip(osip_message_t *sip)
{
	int i;
	char *dest;
	osip_uri_t *toUri = osip_to_get_url(sip->to);
	if (!toUri) {
		LOG(ERR) << "osip_to_get_url problem";
		return "";
	}
	i = osip_uri_to_str(toUri, &dest);
	string imsi = dest;
	osip_free(dest);
	return imsi;
}

// is imsi in the database?
bool imsiFound(string imsi)
{
	string x = imsiGet(imsi, "id");
	return x.length() != 0;
}

string imsiClean(string imsi)
{
	// remove leading sip:
	if (0 == strncasecmp(imsi.c_str(), "sip:", 4)) {
		imsi = imsi.substr(4);
	}
	// remove trailing @...
	size_t p = imsi.find("@");
	if (p != string::npos) {
		imsi = imsi.substr(0, p);
	}
	// remove leading IMSI
	if (0 == strncasecmp(imsi.c_str(), "imsi", 4)) {
		imsi = imsi.substr(4);
	}
	return imsi;
}


char *processBuffer(char *buffer)
{
	int i;

	// parse sip message
	osip_message_t *sip;
	i=osip_message_init(&sip);
	if (i!=0) {
		LOG(ERR) << "cannot allocate";
		osip_message_free(sip);
		return NULL;
	}
	i=osip_message_parse(sip, buffer, strlen(buffer));
	if (i!=0) {
		LOG(ERR) << "cannot parse sip message";
		osip_message_free(sip);
		return NULL;
	}

	prettyPrint("request", sip);

	// response starts as clone of message
	osip_message_t *response;
	osip_message_clone(sip, &response);

	osip_from_t * contact_header = (osip_from_t*)osip_list_get(&sip->contacts,0);
	osip_uri_t* contact_url = contact_header->url; 
	char *remote_host = contact_url->host;

	// return via
	ostringstream newvia;
	// newvia << "SIP/2.0/UDP localhost:5063;branch=1;received=string_address@foo.bar";
	const char *my_ipaddress = "localhost";
	newvia << "SIP/2.0/UDP " << my_ipaddress << ":" << my_udp_port << ";branch=1;received="
		<< "string_address@foo.bar"; // << my_network.string_addr((struct sockaddr *)netaddr, netaddrlen, false);
	osip_message_append_via(response, newvia.str().c_str());

	// no method
	osip_message_set_method(response, NULL);

	string imsi = imsiClean(imsiFromSip(sip));
	string imsiTo = imsiClean(imsiToSip(sip));
	if ((imsi == "EXIT") && (imsiTo == "EXIT")) exit(0); // for testing only
	if (!imsiFound(imsi)) {
		LOG(NOTICE) << "imsi unknown";
		// imsi problem => 404 IMSI Not Found
		osip_message_set_status_code (response, 404);
		osip_message_set_reason_phrase (response, osip_strdup("IMSI Not Found"));
	} else {
		// look for rand and sres in Authorization header (assume imsi same as in from)
		string randx;
		string sres;
		// sip parser is not working reliably for Authorization, so we'll do the parsing
		char *RAND = strcasestr(buffer, "nonce=");
		char *SRES = strcasestr(buffer, "response=");
		if (RAND && SRES) {
			// find RAND digits
			RAND += 6;
			while (!isalnum(*RAND)) { RAND++; }
			RAND[32] = 0;
			int j=0;
			while(isalnum(RAND[j])) { j++; }
			RAND[j] = '\0';
			// find SRES digits
			SRES += 9;
			while (!isalnum(*SRES)) { SRES++; }
			int i=0;
			while(isalnum(SRES[i])) { i++; }
			SRES[i] = '\0';
			LOG(INFO) << "rand = /" << RAND << "/";
			LOG(INFO) << "sres = /" << SRES << "/";
		}
		if (!RAND || !SRES) {
			LOG(NOTICE) << "imsi known, 1st register";
			// no rand and sres => 401 Unauthorized
			osip_message_set_status_code (response, 401);
			osip_message_set_reason_phrase (response, osip_strdup("Unauthorized"));
			// but include rand in www_authenticate
			osip_www_authenticate_t *auth;
			osip_www_authenticate_init(&auth);
			// auth type is required by osip_www_authenticate_to_str (and therefore osip_message_to_str)
			string auth_type = "Digest";
			osip_www_authenticate_set_auth_type(auth, osip_strdup(auth_type.c_str()));
			// returning RAND in www_authenticate header
			string randz = generateRand(imsi);
			osip_www_authenticate_set_nonce(auth, osip_strdup(randz.c_str()));
			i = osip_list_add (&response->www_authenticates, auth, -1);
			if (i < 0) LOG(ERR) << "problem adding www_authenticate";
		} else {
			string kc;
			bool sres_good = authenticate(imsi, RAND, SRES, &kc);
			LOG(INFO) << "imsi known, 2nd register, good = " << sres_good;
			if (sres_good) {
				// sres matches rand => 200 OK
				osip_message_set_status_code (response, 200);
				osip_message_set_reason_phrase (response, osip_strdup("OK"));
				// And register it.
				LOG(INFO) << "success, registering for IP address " << remote_host;
				imsiSet(imsi,"ipaddr",remote_host);
				imsiSet(imsi,"port","5062");
			} else {
				// sres does not match rand => 401 Unauthorized
				osip_message_set_status_code (response, 401);
				osip_message_set_reason_phrase (response, osip_strdup("Unauthorized"));
			}
		}
	}

	prettyPrint("response", response);
	size_t length = 0;
	char *dest;
	int ii = osip_message_to_str(response, &dest, &length);
	if (ii != 0) {
		LOG(ERR) << "cannot get printable message";
	}

	osip_message_free(sip);
	osip_message_free(response);

	return dest;
}


#define BUFLEN 5000

int
main(int argc, char **argv)
{
	sockaddr_in si_me;
	sockaddr_in si_other;
	int aSocket;
	char buf[BUFLEN];

	LOG(ALERT) << argv[0] << " (re)starting";
	srand ( time(NULL) + (int)getpid() );
	my_udp_port = gConfig.getNum("SubscriberRegistry.Port");

	// init osip lib
	osip_t *osip;
	int i=osip_init(&osip);
	if (i!=0) {
		LOG(ALERT) << "cannot init sip lib";
		return NULL;
	}

	if ((aSocket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1) {
		LOG(ALERT) << "can't initialize socket";
		exit(1);
	}

	memset((char *) &si_me, 0, sizeof(si_me));
	si_me.sin_family = AF_INET;
	si_me.sin_port = htons(my_udp_port);
	si_me.sin_addr.s_addr = htonl(INADDR_ANY);
	if (bind(aSocket, (sockaddr*)&si_me, sizeof(si_me)) == -1) {
		LOG(ALERT) << "can't bind socket on port " << my_udp_port;
		exit(1);
	}

	LOG(NOTICE) << "binding on port " << my_udp_port;

	while (true) {
		gConfig.purge();
		socklen_t slen = sizeof(si_other);
		memset(buf, 0, BUFLEN);
		if (recvfrom(aSocket, buf, BUFLEN, 0, (sockaddr*)&si_other, &slen) == -1) {
			LOG(ERR) << "recvfrom problem";
			continue;
		}

		LOG(INFO) << " receiving " << buf;

		char *dest = processBuffer(buf);
		if (dest == NULL) {
			continue;
		}

		if (sendto(aSocket, dest, strlen(dest), 0, (sockaddr*)&si_other, sizeof(si_other)) == -1) {
			LOG(ERR) << "sendto problem";
			continue;
		}
		osip_free(dest);
	}

	close(aSocket);
	return 0;
}
