/*
* Copyright 2012 Range Networks, Inc.
*
* Written by Kurtis Heimerl, November 2012
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

/* This class stores messages for later replay if SMQ fails */

#ifndef DISK_BACKUP_H
#define DISK_BACKUP_H

#include <Logger.h>
#include <sys/time.h>
#include <string.h>
#include "sqlite3util.h"

using namespace std;

typedef struct {
        long long timestamp;
        string text;
} backup_msg;

typedef std::list<backup_msg> backup_msg_list;

//kurtis utility function
long long get_msecs();

class SQLiteBackup {

	private:

	sqlite3 *mDB;			///< database connection


	public:

	~SQLiteBackup();

	/**
			Initialize the backup using parameters from gConfig.
			@return 0 if the database was successfully opened and initialized; 1 otherwise
	*/
	int init();

	typedef enum {
		SUCCESS=0,		///< operation successful
		FAILURE=1,		///< operation not successful
		DELAYED=2,		///< operation successful, but effect delayed
		TRYAGAIN=3		///< operation not attempted, try again later
	} Status;


	sqlite3 *db()
	{
		return mDB;
	}

	/* get all the current elements in the db */
	/* responsibility on caller to delete each entry as well as the list
	   -kurtis */
	backup_msg_list* get_stored_messages();

	/* insert a message into the storage */
	int insert(long long timestamp, char* text);

	/* remove an element from storage */
	int remove(long long timestamp);

};

#endif //diskbackup.h
