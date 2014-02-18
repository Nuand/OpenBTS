/*
* Copyright 2010 Kestrel Signal Processing, Inc.
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


#include "sqlite3.h"
#include "sqlite3util.h"

#include <string.h>
#include <unistd.h>
#include <stdio.h>


// Wrappers to sqlite operations.
// These will eventually get moved to commonlibs.

int sqlite3_prepare_statement(sqlite3* DB, sqlite3_stmt **stmt, const char* query)
{
	int prc = sqlite3_prepare_v2(DB,query,strlen(query),stmt,NULL);
	if (prc) {
		fprintf(stderr,"sqlite3_prepare_v2 failed for \"%s\": %s\n",query,sqlite3_errmsg(DB));
		sqlite3_finalize(*stmt);
	}
	return prc;
}

int sqlite3_run_query(sqlite3* DB, sqlite3_stmt *stmt)
{
	int src = SQLITE_BUSY;
	while (src==SQLITE_BUSY) {
		src = sqlite3_step(stmt);
		if (src==SQLITE_BUSY) {
			usleep(100000);
		}
	}
	if ((src!=SQLITE_DONE) && (src!=SQLITE_ROW)) {
		fprintf(stderr,"sqlite3_run_query failed: %s\n",sqlite3_errmsg(DB));
	}
	return src;
}


bool sqlite3_exists(sqlite3* DB, const char *tableName,
		const char* keyName, const char* keyData)
{
	size_t stringSize = 100 + strlen(tableName) + strlen(keyName) + strlen(keyData);
	char query[stringSize];
	sprintf(query,"SELECT * FROM %s WHERE %s == \"%s\"",tableName,keyName,keyData);
	// Prepare the statement.
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(DB,&stmt,query)) return false;
	// Read the result.
	int src = sqlite3_run_query(DB,stmt);
	sqlite3_finalize(stmt);
	// Anything there?
	return (src == SQLITE_ROW);
}



bool sqlite3_single_lookup(sqlite3* DB, const char *tableName,
		const char* keyName, const char* keyData,
		const char* valueName, unsigned &valueData)
{
	size_t stringSize = 100 + strlen(valueName) + strlen(tableName) + strlen(keyName) + strlen(keyData);
	char query[stringSize];
	sprintf(query,"SELECT %s FROM %s WHERE %s == \"%s\"",valueName,tableName,keyName,keyData);
	// Prepare the statement.
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(DB,&stmt,query)) return false;
	// Read the result.
	int src = sqlite3_run_query(DB,stmt);
	bool retVal = false;
	if (src == SQLITE_ROW) {
		valueData = (unsigned)sqlite3_column_int64(stmt,0);
		retVal = true;
	}
	sqlite3_finalize(stmt);
	return retVal;
}


// This function returns an allocated string that must be free'd by the caller.
bool sqlite3_single_lookup(sqlite3* DB, const char* tableName,
		const char* keyName, const char* keyData,
		const char* valueName, char* &valueData)
{
	valueData=NULL;
	size_t stringSize = 100 + strlen(valueName) + strlen(tableName) + strlen(keyName) + strlen(keyData);
	char query[stringSize];
	sprintf(query,"SELECT %s FROM %s WHERE %s == \"%s\"",valueName,tableName,keyName,keyData);
	// Prepare the statement.
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(DB,&stmt,query)) return false;
	// Read the result.
	int src = sqlite3_run_query(DB,stmt);
	bool retVal = false;
	if (src == SQLITE_ROW) {
		const char* ptr = (const char*)sqlite3_column_text(stmt,0);
		if (ptr) valueData = strdup(ptr);
		retVal = true;
	}
	sqlite3_finalize(stmt);
	return retVal;
}


// This function returns an allocated string that must be free'd by tha caller.
bool sqlite3_single_lookup(sqlite3* DB, const char* tableName,
		const char* keyName, unsigned keyData,
		const char* valueName, char* &valueData)
{
	valueData=NULL;
	size_t stringSize = 100 + strlen(valueName) + strlen(tableName) + strlen(keyName) + 20;
	char query[stringSize];
	sprintf(query,"SELECT %s FROM %s WHERE %s == %u",valueName,tableName,keyName,keyData);
	// Prepare the statement.
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(DB,&stmt,query)) return false;
	// Read the result.
	int src = sqlite3_run_query(DB,stmt);
	bool retVal = false;
	if (src == SQLITE_ROW) {
		const char* ptr = (const char*)sqlite3_column_text(stmt,0);
		if (ptr) valueData = strdup(ptr);
		retVal = true;
	}
	sqlite3_finalize(stmt);
	return retVal;
}




bool sqlite3_command(sqlite3* DB, const char* query)
{
	// Prepare the statement.
	sqlite3_stmt *stmt;
	if (sqlite3_prepare_statement(DB,&stmt,query)) return false;
	// Run the query.
	int src = sqlite3_run_query(DB,stmt);
	sqlite3_finalize(stmt);
	return src==SQLITE_DONE;
}



