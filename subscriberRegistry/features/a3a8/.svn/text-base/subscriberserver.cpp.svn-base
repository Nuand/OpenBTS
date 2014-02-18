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

#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <map>
#include <vector>
#include <string>
#include <sqlite3.h>
#include <time.h>
#include "Configuration.h"
#include "Logger.h"
#include <string.h>
#include "servershare.h"
#include "SubscriberRegistry.h"


using namespace std;

ConfigurationTable gConfig("/etc/OpenBTS/OpenBTS.db");
Log dummy("subscriberserver",gConfig.getStr("Log.Level").c_str(),LOG_LOCAL7);

// just using this for the database access
SubscriberRegistry gSubscriberRegistry;

// map of http query parameters and values
map<string,string> gArgs;
// lines of http response
vector<string> gResponse;



// retrieve a query field from args
string getArg(string label)
{
	if (gArgs.find(label) == gArgs.end()) {
		LOG(ERR) << "error: " << label << " missing";
		return "";
	}
	return gArgs[label];
}

// run an sql statement through sqlite3 to get a response
void generateSqlResponse()
{
	string stmts = getArg("stmts");
	vector<string> vstmts;
	split(';', stmts, &vstmts);
	vector<string>::iterator it;
	for (it = vstmts.begin(); it != vstmts.end(); it++) {
		sqlite3_stmt *stmt;
		if (sqlite3_prepare_statement(gSubscriberRegistry.db(), &stmt, it->c_str())) {
			LOG(ERR) << "sqlite3_prepare_statement problem - statement: " << it->c_str();
			return;
		}
		int src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
		while (src == SQLITE_ROW) {
			string resp = "res=";
			int cols = sqlite3_column_count(stmt);
			for (int i = 0; i < cols; i++) {
				resp.append((const char*)sqlite3_column_text(stmt, i));
			}
			gResponse.push_back(resp);
			src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
		}
	}
}

void sresCheck(bool b)
{
	if (b) {
		// SRESs match. return success (or Kc (TODO))
		gResponse.push_back("status=SUCCESS");
	} else {
		// SRESs don't match.  return failure.
		gResponse.push_back("status=FAILURE");
	}
}

// authenticate
void generateAuthResponse()
{
	string imsi = getArg("imsi");
	string randx = getArg("rand");
	string sres = getArg("sres");
	string kc;
	bool st = authenticate(imsi, randx, sres, &kc);
	sresCheck(st);
}

// generate a 128' random number
void generateRandResponse()
{
	string imsi = getArg("imsi");
	string randx = generateRand(imsi);
	gResponse.push_back("rand=" + randx);
	gResponse.push_back("imsi=" + imsi);
}

// generate our http response, putting each line of it into vector response
void generateResponse()
{
	if (gArgs.find("req") == gArgs.end()) {
		LOG(ERR) << "req not specified";
		return;
	}
	// check for request types
	string req = gArgs["req"];
	if (req == "sql") {
		generateSqlResponse();
		return;
	}
	if (req == "rand") {
		generateRandResponse();
		return;
	}
	if (req == "auth")  {
		generateAuthResponse();
		return;
	}
	// if none of the above, then it's an error
	LOG(ERR) << "unrecognized request";
}

// write the query to the log file
void logQuery()
{
	// list query key->value pairs
	map<string,string>::iterator it;
	LOG(INFO) << "query";
	for (it = gArgs.begin(); it != gArgs.end(); it++) {
		LOG(INFO) << "   " << it->first << " -> " << it->second;
	}
}

// write the http response from the global array @response to the log file
void logResponse()
{
	LOG(INFO) << "response";
	vector<string>::iterator it;
	for (it = gResponse.begin(); it != gResponse.end(); it++) {
		LOG(INFO) << "   " << *it;
	}
}

// print the http response to stdout
void respond()
{
	vector<string>::iterator it;
	for (it = gResponse.begin(); it != gResponse.end(); it++) {
		cout << *it << endl;
	}
}

int main()
{
	cout << "Content-Type: text\n\n";
	srand ( time(NULL) + (int)getpid() );
	// decode the http query
	decodeQuery(gArgs);
	// write the http query into the log file
	logQuery();
	// put the http response into the global array @response
	generateResponse();
	// write the http response to the log file
	logResponse();
	// print out the http response to stdout
	respond();
}
