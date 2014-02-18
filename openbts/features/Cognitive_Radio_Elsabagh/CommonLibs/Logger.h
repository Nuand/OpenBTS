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


#ifndef LOGGER_H
#define LOGGER_H

#include <stdint.h>
#include <stdio.h>
#include <sstream>
#include <list>
#include <map>
#include <string>
#include "Threads.h"


#define DEFAULT_LOGGING_LEVEL "INFO"
#define DEFAULT_ALARM_PORT 10101
#define DEFAULT_MAX_ALARMS 10

#define _LOG(level) \
	Log(Log::LOG_##level).get() << pthread_self() \
	<< " " __FILE__  ":"  << __LINE__ << ":" << __FUNCTION__ << ": "
#define LOG(wLevel) \
	if (gLoggingLevel(__FILE__)>=Log::LOG_##wLevel) _LOG(wLevel)
#define OBJLOG(wLevel) \
	if (gLoggingLevel(__FILE__)>=Log::LOG_##wLevel) _LOG(wLevel) << "obj: " << this << ' '

#define LOG_ASSERT(x) { if (!(x)) LOG(ALARM) << "assertion " #x " failed"; } assert(x);




/**
	A thread-safe logger, directable to any file or stream.
	Derived from Dr. Dobb's Sept. 2007 issue.
	This object is NOT the global logger;
	every log record is an object of this class.
*/
class Log {

	public:

	/** Available logging levels. */
	enum Level {
		LOG_FORCE,
		LOG_ERROR,
		LOG_ALARM,
		LOG_WARN,
		LOG_NOTICE,
		LOG_INFO,
		LOG_DEBUG,
		LOG_DEEPDEBUG
	};

	protected:

	std::ostringstream mStream;	///< This is where we write the long.
	Level mReportLevel;			///< Level of current repot.

	static FILE *sFile;

	public:

	Log(Level wReportLevel)
		:mReportLevel(wReportLevel)
	{ }

	// Most of the work in in the desctructor.
	~Log();

	std::ostringstream& get();
};

std::ostringstream& operator<<(std::ostringstream& os, Log::Level);



std::list<std::string> gGetLoggerAlarms();		///< Get a copy of the recent alarm list.


/**@ Global control and initialization of the logging system. */
//@{
void gLogInit(const char* defaultLevel = DEFAULT_LOGGING_LEVEL);
Log::Level gLoggingLevel(const char *filename);
//@}

/**@name Global logging file control. */
//@{
void gSetLogFile(FILE*);
bool gSetLogFile(const char*);
//@}


#endif

// vim: ts=4 sw=4
