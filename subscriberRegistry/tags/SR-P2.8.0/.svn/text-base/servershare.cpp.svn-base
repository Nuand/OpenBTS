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


#include <iostream>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <Configuration.h>
#include <string.h>

#include "servershare.h"
#include "sqlite3.h"
#include "Logger.h"
#include "SubscriberRegistry.h"

using namespace std;


extern ConfigurationTable gConfig;

// just using this for the database access
extern SubscriberRegistry gSubscriberRegistry;



string imsiGet(string imsi, string key)
{
	string name = imsi.substr(0,4) == "IMSI" ? imsi : "IMSI" + imsi;
	char *value;
	if (!sqlite3_single_lookup(gSubscriberRegistry.db(), "sip_buddies", "name", name.c_str(), key.c_str(), value)) {
		return "";
	}
	if (!value) { return ""; }
	string retValue = value;
	free(value);
	return retValue;
}

void imsiSet(string imsi, string key, string value)
{
	string name = imsi.substr(0,4) == "IMSI" ? imsi : "IMSI" + imsi;
	ostringstream os2;
	os2 << "update sip_buddies set " << key << " = \"" << value << "\" where name = \"" << name << "\"";
	if (!sqlite3_command(gSubscriberRegistry.db(), os2.str().c_str())) {
		LOG(ERR) << "sqlite3_command problem";
		return;
	}
}

string soGenerateIt()
{
	ostringstream os;
	for (int i = 0; i < 32; i++) {
		// if rand() is too slow you can call it fewer times
		os << hex << (rand() & 0xf);
	}
	return os.str();
}



// generate a 128' random number
string generateRand(string imsi)
{
	string ki = imsiGet(imsi, "ki");
	string ret;
	if (ki.length() != 0) {
		LOG(INFO) << "ki is known";
		// generate and return rand (clear any cached rand or sres)
		imsiSet(imsi, "rand", "");
		imsiSet(imsi, "sres", "");
		ret = soGenerateIt();
	} else {
		string wRand = imsiGet(imsi, "rand");
		if (wRand.length() != 0) {
			LOG(INFO) << "ki is unknown, rand is cached";
			// return cached rand
			ret = wRand;
		} else {
			LOG(INFO) << "ki is unknown, rand is not cached";
			// generate rand, cache rand, clear sres, and return rand
			wRand = soGenerateIt();
			imsiSet(imsi, "rand", wRand);
			imsiSet(imsi, "sres", "");
			ret = wRand;
		}
	}
	return ret;
}

bool strEqual(string a, string b)
{
	return 0 == strcasecmp(a.c_str(), b.c_str());
}

void decodeQuery(map<string,string> &args)
{
	string query;
	// this works for GET or POST.
	// get the request method
	char *g = getenv("REQUEST_METHOD");
	string method = g ? g : "";
	LOG(INFO) << "REQUEST_METHOD = " << g;
	// if POST, then read from stdin the number of bytes specified in CONTENT_LENGTH, and that's the query
	if (method == "POST") {
		int lth = atoi(getenv("CONTENT_LENGTH"));
		LOG(INFO) << "CONTENT_LENGTH = " << lth;
		char *buf = new char[lth+1];
		cin.get(buf, lth+1);
		int nread = cin.gcount();
		if (nread != lth) {
			LOG(ERR) << "content length changed to " << nread;
			lth = nread;
		}
		query = string(buf, lth);
		LOG(INFO) << "QUERY = " << query;
		delete[] buf;
	// if GET, then the query is in the environment variable QUERY_STRING
	} else if (method == "GET") {
		char *q = getenv("QUERY_STRING");
		query = q ? q : "";
		LOG(INFO) << "QUERY_STRING = " << q;
	}
	if (query.length() != 0) {
		// fields of http request are separated with "&"
		vector<string> fields;
		split('&', query, &fields);
		vector<string>::iterator it;
		for (it = fields.begin(); it != fields.end(); it++) {
			string field = *it;
			size_t p = field.find('=');
			string key = field.substr(0, p);
			string value = field.substr(p+1);
			p = 0;
			while (1) {
				size_t q = value.find('%', p);
				if (q == string::npos) break;
				string hex = value.substr(q+1, 2);
				char s[2];
				strcpy(s, "x");
				int i;
				sscanf(hex.c_str(), "%x", &i);
				s[0] = i;
				string hexx = s;
				value.replace(q, 3, hexx);
			}
			args[key] = value;
		}
	}
}

string join(string separator, vector<string> &strings)
{
	string result("");
	vector<string>::iterator it;
	for (it = strings.begin(); it != strings.end(); it++) {
		if (it != strings.begin()) result.append(separator);
		result.append(*it);
	}
	return result;
}

void split(char separator, string tosplit, vector<string> *fields)
{
	int p = 0;
	while (1) {
		size_t q = tosplit.find(separator, p);
		if (q == string::npos) {
			fields->push_back(tosplit.substr(p));
			break;
		}
		fields->push_back(tosplit.substr(p, q-p));
		p = q+1;
	}
}
