/*
* Copyright 2008, 2009, 2010 Free Software Foundation, Inc.
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


#include "Configuration.h"
#include <fstream>
#include <iostream>
#include <string.h>

using namespace std;

bool ConfigurationTable::readFile(const char* filename)
{
	ifstream configFile(filename);
	if (!configFile) {
		cerr << "cannot open configuration file " << filename << endl;
		return false;
	}
	while (configFile) {
		string thisLine;
		getline(configFile,thisLine);
		if (!configFile) break;
		// Skip leading spaces.
		int i=0;
		while (thisLine[i]==' ') i++;
		// Skip comments
		if (thisLine[i]=='#') continue;
		// Skip blank lines
		if (thisLine[i]=='\0') continue;
		// Catch directives
		if (thisLine[i]=='$') {
			processDirective(thisLine);
			continue;
		}
		// Tokenize and put in the table.
		string::size_type pos = thisLine.find_first_of(" ",i);
		string key = thisLine.substr(i,pos);
		if (pos==string::npos) {
			mTable[key]="";
			continue;
		}
		string value = thisLine.substr(pos+1);
		mTable[key]=value;
	}
	configFile.close();
	return true;
}


void ConfigurationTable::processDirective(const string& thisLine)
{
	string::size_type pos = thisLine.find_first_of(" ");
	string key = thisLine.substr(pos+1);
	string directive = thisLine.substr(1,pos-1);

	if (directive=="static") {
		if (!defines(key)) {
			cerr << "non-existent key " << key << " cannot be static" << endl;
			throw ConfigurationTableKeyNotFound(key);
		}
		mStatic[key] = true;
		return;
	}

	if (directive=="optional") {
		// It's OK for a non-existant key to be optional.
		mOptional[key] = true;
		return;
	}

	cerr << "invalid configuration directive " << thisLine << endl;
}



bool ConfigurationTable::defines(const string& key) const
{
	StringMap::const_iterator where = mTable.find(key);
	return (where!=mTable.end());
}



bool ConfigurationTable::isStatic(const string& key) const
{
	StringBoolMap::const_iterator where = mStatic.find(key);
	if (where==mStatic.end()) return false;
	return where->second;
}

bool ConfigurationTable::isRequired(const string& key) const
{
	StringBoolMap::const_iterator where = mOptional.find(key);
	if (where==mOptional.end()) return true;
	return !(where->second);
}




const char* ConfigurationTable::getStr(const string& key) const
{
	StringMap::const_iterator where = mTable.find(key);
	if (where==mTable.end()) throw ConfigurationTableKeyNotFound(key);
	return where->second.c_str();
}


std::vector<unsigned> ConfigurationTable::getVector(const string& key) const
{
	StringMap::const_iterator where = mTable.find(key);
	if (where==mTable.end()) throw ConfigurationTableKeyNotFound(key);
	// Make an alterable copy of the string.
	char* line = strdup(where->second.c_str());
	std::vector<unsigned> retVal;
	char *lp=line;
	while (lp) {
		retVal.push_back(strtol(lp,NULL,10));
		strsep(&lp," ");
	}
	free(line);
	return retVal;
}


bool ConfigurationTable::unset(const string& key)
{
	if (isStatic(key)) return false;
	if (isRequired(key)) return false;
	StringMap::iterator where = mTable.find(key);
	if (where==mTable.end()) return false;
	mTable.erase(where);
	return true;
}


void ConfigurationTable::dump(ostream& os) const
{
	StringMap::const_iterator cfg = mTable.begin();
	while (cfg != mTable.end()) {
		os << cfg->first << " " << cfg->second << endl;
		++cfg;
	}
}

void ConfigurationTable::write(ostream& os) const
{
	StringMap::const_iterator cfg = mTable.begin();
	while (cfg != mTable.end()) {
		os << endl;
		os << cfg->first << " " << cfg->second << endl;
		if (isStatic(cfg->first)) os << "$static " << cfg->first << endl;
		if (!isRequired(cfg->first)) os << "$optional " << cfg->first << endl;
		++cfg;
	}
}


bool ConfigurationTable::set(const string& key, const string& value)
{
	if (isStatic(key)) return false;
	mTable[key]=value;
	return true;
}

bool ConfigurationTable::set(const string& key, long value)
{
	char buffer[30];
	sprintf(buffer,"%ld",value);
	return set(key,buffer);
}


// vim: ts=4 sw=4
