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
#include <algorithm>

using namespace std;

ConfigurationTable gConfig("/etc/OpenBTS/OpenBTS.db");
map<string,string> gArgs;
string gDatabase;
string gVisibleSipColumns;
string gUrl;
string gTitle;
string gVisibleExtColumns = "exten dial";

// just using this for the database access
SubscriberRegistry gSubscriberRegistry;


#define NO_BUTTON 0
#define UPDATE_BUTTON 1
#define ADD_BUTTON 2
#define DELETE_BUTTON 4

// may need some way to add/update a NULL field
void tableRow(vector<string> &names, vector<string> &values, int buttons, string id)
{
	cout << "<form name=\"input\" action=\"" << gUrl << "\" method=\"post\">\n";
	for (size_t i = 0; i < names.size(); i++) {
		string name = names[i];
		string value = values.size() == 0 ? "" : values[i];
		cout << "<input type=\"text\" name=\"" << name << "\" value=\"" << value << "\" />\n";
	}
	cout << "<input type=\"hidden\" name=\"id\" value=\"" << id << "\" />\n";
	if (buttons & UPDATE_BUTTON) {
		cout << "<input type=\"submit\" name=\"what\" value=\"Update\" />\n";
	}
	if (buttons & ADD_BUTTON) {
		cout << "<input type=\"submit\" name=\"what\" value=\"Add\" />\n";
	}
	if (buttons & DELETE_BUTTON) {
		cout << "<input type=\"submit\" name=\"what\" value=\"Delete\" />\n";
	}
	cout << "</form>\n";
}

// make the header a fake form to get the same widths
void initTable(vector<string> &cols)
{
	tableRow(cols, cols, NO_BUTTON, "0");
}

void table(const char *tableName, vector<string> &cols, bool addButtonP, const char *note)
{
	cout << "<h4>" << gDatabase << "." << tableName << "  (" << note << ")</h4>\n";
	initTable(cols);
	ostringstream os;
	os << "select id," << join(",", cols) << " from " << tableName;
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(gSubscriberRegistry.db(), &stmt,os.str().c_str())) {
		cout << "sqlite3_prepare_statement problem" << endl;
		return;
	}
	int src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
	while (src == SQLITE_ROW) {
		vector<string> values;
		const char *id = (const char*)sqlite3_column_text(stmt, 0);
		for (int i = 1; i <= (int)cols.size(); i++) {
			const char *value = (const char*)sqlite3_column_text(stmt, i);
			values.push_back(value ? value : "-NULL-");
		}
		tableRow(cols, values, UPDATE_BUTTON | DELETE_BUTTON, id);
		src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
	}
	sqlite3_finalize(stmt);
	if (addButtonP) {
		vector<string> dummy;
		tableRow(cols, dummy, ADD_BUTTON, "0");
	}
}

void getFields(vector<string> *fields, vector<bool> *isSet)
{ 
	vector<string> vsc;
	split(' ', gVisibleSipColumns, &vsc);
	sqlite3_stmt *stmt;
	const char *cmd = "pragma table_info(sip_buddies)";
	if (sqlite3_prepare_statement(gSubscriberRegistry.db(), &stmt, cmd)) {
		LOG(ERR) << "sqlite3_prepare_statement problem - statement: " << cmd;
		return;
	}
	int src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
	while (src == SQLITE_ROW) {
		string field = (char*)sqlite3_column_text(stmt, 1);
		fields->push_back(field);
		isSet->push_back(find(vsc.begin(), vsc.end(), field) != vsc.end());
		src = sqlite3_run_query(gSubscriberRegistry.db(), stmt);
	}
	sqlite3_finalize(stmt);
}

void mainTables()
{
	cout << "<form name=\"provision\" action=\"" << gUrl << "\" method=\"post\">\n";
	cout << "Phone Number = ";
	cout << "<input type=\"text\" name=\"phonenumber\" value=\"\" />\n";
	cout << "IMSI = ";
	cout << "<input type=\"text\" name=\"imsi\" value=\"\" />\n";
	cout << "<input type=\"submit\" name=\"what\" value=\"Provision\" />\n";
	cout << "</form>\n";
	cout << "<br><hr><br>\n";

	vector<string> vsc;
	split(' ', gVisibleSipColumns, &vsc);
	table("sip_buddies", vsc, false, "scroll down to change which fields of sip_buddies are visible");
	cout << "<hr>";
	vector<string> vec;
	split(' ', gVisibleExtColumns, &vec);
	table("dialdata_table", vec, true, "");

	cout << "<br><hr><br>\n";
	cout << "<FORM METHOD=\"LINK\" ACTION=\"" << gUrl << "\"> <INPUT TYPE=\"submit\" VALUE=\"Refresh\"> </FORM>\n";

	cout << "<br><hr><br>\n";
	cout << "<h4>Selected fields are included in sip_buddies table (submit button at bottom)</h4>\n";
	cout << "<form method=\"post\" action=\"" << gUrl << "\">\n";
	vector<string> fields;
	vector<bool> isSet;
	getFields(&fields, &isSet);
	for (int i = 0; i < (int)fields.size(); i++) {
		string field = fields[i];
		string checked = isSet[i] ? "checked" : "";
		cout << "<input type=\"checkbox\" name=\"" << field << "\" value=\"" << field << "\"" << checked << " /> " << field << "<br />\n";
	}
	cout << "<br><INPUT TYPE=SUBMIT name=\"what\" VALUE=\"Submit\">\n";
	cout << "</form>\n";
}

void doCmd(string cmd)
{
	string table;
	vector<string> cols;
	if (gArgs.find("dial") == gArgs.end()) {
		table = "sip_buddies";
		split(' ', gVisibleSipColumns, &cols);
	} else {
		table = "dialdata_table";
		split(' ', gVisibleExtColumns, &cols);
	}
	string id = gArgs["id"];
	ostringstream os;
	if (cmd == "add") {
		string names = join(",", cols);
		vector<string> values0;
		vector<string>::iterator it;
		for (it = cols.begin(); it != cols.end(); it++) {
			values0.push_back("\"" + gArgs[*it] + "\"");
		}
		string values = join(",", values0);
		os << "insert into " << table << " (" << names << ") values (" << values << ")";
	} else if (cmd == "delete") {
		os << "delete from " << table << " where id = " << id;
	} else if (cmd == "update") {
		vector<string> sets0;
		vector<string>::iterator it;
		for (it = cols.begin(); it != cols.end(); it++) {
			sets0.push_back(*it + "=\"" + gArgs[*it] + "\"");
		}
		string sets = join(",", sets0);
		os << "update " << table << " set " << sets << " where id = " << id;
	} else {
		cout << "internal error<br>\n";
	}
	if (!sqlite3_command(gSubscriberRegistry.db(), os.str().c_str())) {
		cout << "sqlite3_command problem" << endl;
		return;
	}
	mainTables();
}

void initHtml()
{
	cout << "Content-Type: text/html\n\n";
	cout << "<HTML>\n";
	cout << "<HEAD>\n";
	cout << "<TITLE>" << gTitle << "</TITLE>\n";
	cout << "</HEAD>\n";
	cout << "<BODY>\n";
	cout << "<h4>" << gTitle << "</h4>\n";
	time_t rawtime;
	time ( &rawtime );
	struct tm * timeinfo;
	timeinfo = localtime ( &rawtime );
	cout << "<h4>" << asctime(timeinfo) << "</h4>\n";
}

void endHtml()
{
	cout << "</BODY>\n";
	cout << "</HTML>\n";
}

int main(int argc, char **argv)
{
	gLogInit("srmanager",gConfig.getStr("Log.Level").c_str(),LOG_LOCAL7);
	gSubscriberRegistry = SubscriberRegistry();
	// start the html return
	initHtml();
	// read the config file
	gVisibleSipColumns = gConfig.getStr("SubscriberRegistry.Manager.VisibleColumns");
	gUrl = argv[0];
	gTitle = gConfig.getStr("SubscriberRegistry.Manager.Title");
	// connect to the database
	gDatabase = gConfig.getStr("SubscriberRegistry.db");
	// decode the http query
	decodeQuery(gArgs);
	// execute command
	string what = gArgs["what"];
	if (!what.length() || what == "Main") {
		mainTables();
	} else if (what == "Add") {
		doCmd("add");
	} else if (what == "Update") {
		doCmd("update");
	} else if (what == "Delete") {
		doCmd("delete");
	} else if (what == "Provision") {
		gSubscriberRegistry.addUser(gArgs["imsi"].c_str(), gArgs["phonenumber"].c_str());
		mainTables();
	} else if (what == "Submit") {
		gVisibleSipColumns = "";
		map<string,string>::iterator it;
		bool first = true;
		for (it = gArgs.begin(); it != gArgs.end(); it++) {
			if (it->first == "what") continue;
			if (first) {
				first = false;
			} else {
				gVisibleSipColumns += " ";
			}
			gVisibleSipColumns += it->first;
		}
		if (!gConfig.set("SubscriberRegistry.Manager.VisibleColumns", gVisibleSipColumns)) {
			LOG(ERR) << "unable to update SubscriberRegistry.Manager.VisibleColumns";
		}
		mainTables();
	} else {
		cout << "unrecognized what parameter<br>\n";
		map<string,string>::iterator it;
		for (it = gArgs.begin(); it != gArgs.end(); it++) {
			cout << it->first << " -> " << it->second << "<br>\n";
		}
	}
	// finish the html return
	endHtml();
}
