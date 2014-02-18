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


#ifndef CONFIGURATION_H
#define CONFIGURATION_H

#include <assert.h>
#include <map>
#include <vector>
#include <string>
#include <stdlib.h>
#include <iostream>


/** A class for configuration file errors. */
class ConfigurationTableError {};

/** An exception thrown when a given config key isn't found. */
class ConfigurationTableKeyNotFound : public ConfigurationTableError {

	private:

	std::string mKey;

	public:

	ConfigurationTableKeyNotFound(const std::string& wKey)
		:mKey(wKey)
	{ std::cerr << "cannot find configuration value " << mKey << std::endl; }
};



typedef std::map<std::string,std::string> StringMap;
typedef std::map<std::string,bool> StringBoolMap;


/**
	A class for reading a configuration key-value table
	and storing it in a map.
	This class is not thread safe.
	The expectation is that the configuration table will be defined
	once and then used as read-only the rest of the time.
*/
class ConfigurationTable {

	private:

	StringMap mTable;			///< The configuration table
	StringBoolMap mStatic;		///< Flags to indicate static config values.
	StringBoolMap mOptional;	///< Flags to indicate optional config values.

	// Static config values cannot be modified after initial file read.
	// Required config values cannot be removed.

	public:

	bool readFile(const char* filename);

	ConfigurationTable(const char* filename)
		{ assert(readFile(filename)); }

	ConfigurationTable() {}

	/** Return true if the key is used in the table.  */
	bool defines(const std::string& key) const;

	/** Return true if this key is identified as static. */
	bool isStatic(const std::string& key) const;

	/** Make a key static. */
	void makeStatic(const std::string& key) { mStatic[key] = true; }

	/** Return true if this key is identified as required (!optional). */
	bool isRequired(const std::string& key) const;

	/** Make a key optional. */
	void makeOptional(const std::string& key) { mOptional[key]=true; }

	/**
		Get a string parameter from the table.
		Throw ConfigurationTableKeyNotFound if not found.
	*/
	const char *getStr(const std::string& key) const;

	/**
		Get a numeric parameter from the table.
		Throw ConfigurationTableKeyNotFound if not found.
	*/
	long getNum(const std::string& key) const { return strtol(getStr(key),NULL,10); }

	/**
		Get a numeric vector from the table.
	*/
	std::vector<unsigned> getVector(const std::string& key) const;

	/** Set or change a value in the table.  */
	bool set(const std::string& key, const std::string& value);

	/** Set or change a value in the table.  */
	bool set(const std::string& key, long value);

	/**
		Remove a key from the table.
		Will not remove static or required values.
		@param key The key of the item to be deleted.
		@return true if anything was actually removed.
	*/
	bool unset(const std::string &key);

	/** Dump the table to a stream. */
	void dump(std::ostream&) const;

	/** Write the table to a stream, with directives. */
	void write(std::ostream&) const;

	/** Raw iterator. */
	StringMap::const_iterator begin() const { return mTable.begin(); }

	/** End check. */
	StringMap::const_iterator end() const { return mTable.end(); }


	private:

	void processDirective(const std::string& line);

};

#endif


// vim: ts=4 sw=4
