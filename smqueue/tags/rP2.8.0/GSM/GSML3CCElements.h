/**@file Elements for Call Control, GSM 04.08 10.5.4.  */
/*
* Copyright 2008, 2009 Free Software Foundation, Inc.
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




#ifndef GSML3CCELEMENTS_H
#define GSML3CCELEMENTS_H

#include "GSML3Message.h"
#include <iostream>


namespace GSM {



/** A general class for BCD numbers as they normally appear in L3. */
class L3BCDDigits {

	private:

	static const unsigned maxDigits = 20;
	char mDigits[maxDigits+1];					///< ITU-T E.164 limits address to 15 digits

	public:

	L3BCDDigits() { mDigits[0]='\0'; }

	L3BCDDigits(const char* wDigits) { strncpy(mDigits,wDigits,sizeof(mDigits)-1); mDigits[sizeof(mDigits)-1]='\0'; }

	void parse(const L3Frame& src, size_t &rp, size_t numOctets);
	void write(L3Frame& dest, size_t &wp) const;

	/** Return number of octets needed to encode the digits. */
	size_t lengthV() const;

	unsigned size() const { return strlen(mDigits); }
	const char* digits() const { return mDigits; }
};


std::ostream& operator<<(std::ostream&, const L3BCDDigits&);







/** Calling Party BCD Number, GSM 04.08 10.5.4.9 */
class L3CallingPartyBCDNumber : public L3ProtocolElement {

private:

	TypeOfNumber mType;
	NumberingPlan mPlan;

	L3BCDDigits mDigits;

	/**@name Octet 3a */
	//@{
	bool mHaveOctet3a;
	int mPresentationIndicator;
	int mScreeningIndicator;
	//@}


public:

	L3CallingPartyBCDNumber()
		:mType(UnknownTypeOfNumber),mPlan(UnknownPlan),
		mHaveOctet3a(false)
	{ }

	L3CallingPartyBCDNumber( const char * wDigits )
		:mType(NationalNumber),mPlan(E164Plan),mDigits(wDigits),
		mHaveOctet3a(false)
	{ }


	NumberingPlan plan() const { return mPlan; }
	TypeOfNumber type() const { return mType; }
	const char* digits() const { return mDigits.digits(); }

	size_t lengthV() const;
	void writeV( L3Frame& dest, size_t &wp  ) const;
	void parseV( const L3Frame& src, size_t &rp, size_t expectedLength);	
	void parseV(const L3Frame&, size_t&) { assert(0); }
	void text(std::ostream&) const;
};


/** Called Party BCD Number, GSM 04.08 10.5.4.7 */
class L3CalledPartyBCDNumber : public L3ProtocolElement {


private:

	TypeOfNumber mType;
	NumberingPlan mPlan;
	L3BCDDigits mDigits;

public:

	L3CalledPartyBCDNumber()
		:mType(UnknownTypeOfNumber),
		mPlan(UnknownPlan)
	{ }

	L3CalledPartyBCDNumber(const char * wDigits)
		:mType(NationalNumber),mPlan(E164Plan),mDigits(wDigits)
	{ }

	L3CalledPartyBCDNumber(const L3CallingPartyBCDNumber& other)
		:mType(other.type()),mPlan(other.plan()),mDigits(other.digits())
	{ }


	NumberingPlan plan() const { return mPlan; }
	TypeOfNumber type() const { return mType; }
	const char* digits() const { return mDigits.digits(); }

	size_t lengthV() const ; 
	void writeV( L3Frame& dest, size_t &wp  ) const;
	void parseV( const L3Frame& src, size_t &rp, size_t expectedLength );	
	void parseV(const L3Frame&, size_t&) { assert(0); }
	void text(std::ostream&) const;
};








} // GSM

#endif
// vim: ts=4 sw=4
