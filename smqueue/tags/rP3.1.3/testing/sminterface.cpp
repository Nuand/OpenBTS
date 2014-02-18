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
#include <time.h>
#include "Configuration.h"
#include "Logger.h"
#include <string.h>

#include <Sockets.h>


using namespace std;

ConfigurationTable gConfig("/etc/OpenBTS/smqueue.db");
Log dummy("sminterface",gConfig.getStr("Log.Level").c_str(),LOG_LOCAL7);

// map of http query parameters and values
map<string,string> gArgs;
// lines of http response
vector<string> gResponse;


void decodeQuery(map<string,string> &args);
string join(string separator, vector<string> &strings);
void split(char separator, string tosplit, vector<string> *fields);


// retrieve a query field from args
string getArg(string label)
{
	if (gArgs.find(label) == gArgs.end()) {
		LOG(ERR) << "error: " << label << " missing";
		return "";
	}
	return gArgs[label];
}

int sendMessage(const char *smqueueIP, int smqueuePort, const char *myIP, int myPort,
		const char *smscCode, const char *from, const char *to,
		const char *txtBuf)
{
	int returnVal = 0;
	static UDPSocket sock(myPort, smqueueIP, smqueuePort);

	static const char form[] =
		"MESSAGE sip:%s@%s SIP/2.0\n"
		"Via: SIP/2.0/UDP %s;branch=%x\n"
		"Max-Forwards: 2\n"
		"From: %s <sip:%s@%s:%d>;tag=%d\n"
		"To: sip:%s@%s\n"
		"Call-ID: %x@%s:%d\n"
		"CSeq: 1 MESSAGE\n"
		"Content-Type: text/plain\n" \
		"Content-Length: %u\n"
		"\n%s\n";
	static char buffer[1500];
	snprintf(buffer, 1499, form,
		smscCode, smqueueIP,
		myIP, (unsigned)random(),
		from, from, myIP, myPort, (unsigned)random(),
		to, smqueueIP,
		(unsigned)random(), myIP, myPort,
		strlen(txtBuf), txtBuf);
	sock.write(buffer);

	int numRead = sock.read(buffer,10000);
	if (numRead >= 0) {
		buffer[numRead] = '\0';

		printf("%s\n", buffer);
	} else {
		printf("%s\n", "Timed out");
		returnVal = -1;
	}

	return returnVal;
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


void generateSMSSubmit()
{
	string smscIP = getArg("ip");
	int smscPort = atoi(getArg("port").c_str());
	string to = getArg("to");
	string from = getArg("from");
	string message = getArg("message");

	// TODO: Get the IP address
	if (!sendMessage(smscIP.c_str(), smscPort,
			"127.0.0.1", 5070, "smsc",
			from.c_str(), to.c_str(), message.c_str())) {
		gResponse.push_back("result=200");
	}
}

// generate our http response, putting each line of it into vector response
void generateResponse()
{
	if (gArgs.find("req") == gArgs.end()) {
		LOG(ERR) << "req not specified";
		return;
	}

	string req = gArgs["req"];
	if (req == "submit") {
		generateSMSSubmit();
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

