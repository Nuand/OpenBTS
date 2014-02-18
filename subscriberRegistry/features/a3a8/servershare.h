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


#include <string>
#include "sqlite3.h"

using namespace std;

/**
	Get a subscriber's property.
	@param imsi imsi of the subscriber
	@param key name of the property
*/
string imsiGet(string imsi, string key);

/**
	Set a subscriber's property.
	@param imsi imsi of the subscriber
	@param key name of the property
	@param value value of the property
*/
void imsiSet(string imsi, string key, string value);

/**
	Generate a 128-bit random number.
	@param imsi imsi of subscriber the random number is for
*/
string generateRand(string imsi);

/**
	Authenticate
	@param imsi imsi of subscriber
	@param rand random number
	@param sres corresponsing sres
*/
bool authenticate(string imsi, string rand, string sres, string *kc);

/**
	Decode the html query.
	@param args mapping of query key->value pairs.
*/
void decodeQuery(map<string,string> &args);

/**
	Join the strings in strings, separated by separator
	@param separator the separator
	@param strings the strings to join
*/
string join(string separator, vector<string> &strings);

/**
	Split tosplit into strings in fields, using separator
	@param separator the separator to look for
	@param tosplit the string to split
	@param fields the vector result
*/
void split(char separator, string tosplit, vector<string> *fields);

/**
	Open the database whose name is in the config table
*/
sqlite3 *openDB();
