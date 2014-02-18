/**@file @brief Elements for Mobility Management messages, GSM 04.08 9.2. */

/*
* Copyright 2008-2010 Free Software Foundation, Inc.
* Copyright 2010 Kestrel Signal Processing, Inc.
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



#ifndef GSML3MMELEMENTS_H
#define GSML3MMELEMENTS_H

#include "GSML3Message.h"
#include <Globals.h>

namespace GSM {




/**
	Time & Time Zone, GSM 04.08 10.5.3.9, GSM 03.40 9.2.3.11.
	This class is also used in SMS.
*/
class L3TimeZoneAndTime : public L3ProtocolElement {
public:
	enum TimeType {
		LOCAL_TIME, ///< Used in SMS. Time is sent as local time. In this case
		            ///< timezone seems to be ignored by handsets (tested with
		            ///< Nokia DCT3, Siemens and Windows Mobile 6), but we still
		            ///< send it.
		UTC_TIME    ///< Used in MM Info message. Time is sent as UTC time. In
		            ///< this case phones seem to regard timezone information.
	};

protected:

	Timeval mTime;
	TimeType mType;

public:

	/** Defaults from the current time. */
	L3TimeZoneAndTime(const Timeval& wTime = Timeval(), TimeType type = LOCAL_TIME)
		:L3ProtocolElement(),
		mTime(wTime),
		mType(type)
	{}

	const Timeval& time() const { return mTime; }
	void time(const Timeval& wTime) { mTime=wTime; }

	TimeType type() const { return mType; }
	void type(TimeType type) { mType=type; }

	size_t lengthV() const { return 7; }
	void writeV(L3Frame&, size_t&) const;
	void parseV(const L3Frame& src, size_t &rp);
	void parseV(const L3Frame&, size_t& , size_t) { assert(0); }
	void text(std::ostream&) const;
};



} // namespace GSM

#endif

// vim: ts=4 sw=4
