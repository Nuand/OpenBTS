/**@file @brief Call Control messages, GSM 04.08 9.3 */
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




#include "GSML3CCElements.h"
#include <Logger.h>

using namespace std;
using namespace GSM;


void L3BCDDigits::parse(const L3Frame& src, size_t &rp, size_t numOctets)
{
	unsigned i=0;
	size_t readOctets = 0;
	while (readOctets < numOctets) {
		unsigned d2 = src.readField(rp,4);
		unsigned d1 = src.readField(rp,4);
		readOctets++;
		mDigits[i++]=d1+'0';
		if (d2!=0x0f) mDigits[i++]=d2+'0';
		if (i>maxDigits) L3_READ_ERROR;
	}
	mDigits[i++]='\0';
}


void L3BCDDigits::write(L3Frame& dest, size_t &wp) const
{
	unsigned index = 0;
	unsigned numDigits = strlen(mDigits);
	while (index < numDigits) {
		if ((index+1) < numDigits) dest.writeField(wp,mDigits[index+1]-'0',4);
		else dest.writeField(wp,0x0f,4);
		dest.writeField(wp,mDigits[index]-'0',4);
		index += 2;
	}
}


size_t L3BCDDigits::lengthV() const 
{
	unsigned sz = strlen(mDigits);
	return (sz/2) + (sz%2);
}



ostream& GSM::operator<<(ostream& os, const L3BCDDigits& digits)
{
	os << digits.digits();
	return os;
}



void L3CalledPartyBCDNumber::writeV( L3Frame &dest, size_t &wp ) const
{
	dest.writeField(wp, 0x01, 1);
	dest.writeField(wp, mType, 3);
	dest.writeField(wp, mPlan, 4);
	mDigits.write(dest,wp);
}

void L3CalledPartyBCDNumber::parseV( const L3Frame &src, size_t &rp, size_t expectedLength ) 
{
	LOG(DEBUG) << "L3CalledPartyBCDNumber::parseV rp="<<rp<<" expLen="<<expectedLength;
	// ext bit must be 1
	if (src.readField(rp, 1) != 1) L3_READ_ERROR;	
	mType = (TypeOfNumber)src.readField(rp, 3);
	mPlan = (NumberingPlan)src.readField(rp, 4);
	mDigits.parse(src,rp,expectedLength-1);
}


size_t L3CalledPartyBCDNumber::lengthV() const
{
	if (mDigits.lengthV()==0) return 0;
	return 1 + mDigits.lengthV();
}

void L3CalledPartyBCDNumber::text(ostream& os) const
{
	os << "type=" << mType;
	os << " plan=" << mPlan;
	os << " digits=" << mDigits;
}



void L3CallingPartyBCDNumber::writeV( L3Frame &dest, size_t &wp ) const
{
	// If Octet3a is extended, then write 0 else 1.
	dest.writeField(wp, (!mHaveOctet3a & 0x01), 1);
	dest.writeField(wp, mType, 3);
	dest.writeField(wp, mPlan, 4);

	if(mHaveOctet3a){
		dest.writeField(wp, 0x01, 1);
		dest.writeField(wp, mPresentationIndicator, 2); 	
		dest.writeField(wp, 0, 3);
		dest.writeField(wp, mScreeningIndicator, 2);
	}

	mDigits.write(dest,wp);
}





void L3CallingPartyBCDNumber::parseV( const L3Frame &src, size_t &rp, size_t expectedLength) 
{
	size_t remainingLength = expectedLength;
	// Read out first bit = 1.
	mHaveOctet3a = src.readField(rp, 1);	
	mType = (TypeOfNumber)src.readField(rp, 3);
	mPlan = (NumberingPlan)src.readField(rp, 4);
	remainingLength -= 1;

	if (mHaveOctet3a) {
		if (src.readField(rp,1)!=1) L3_READ_ERROR;
		mPresentationIndicator = src.readField(rp, 3);
		src.readField(rp,3);
		mScreeningIndicator = src.readField(rp, 4);
		remainingLength -= 1;
	}

	mDigits.parse(src,rp,remainingLength);
}


size_t L3CallingPartyBCDNumber::lengthV() const
{
	return 1 + mHaveOctet3a + mDigits.lengthV();
}



void L3CallingPartyBCDNumber::text(ostream& os) const
{
	os << "type=" << mType;
	os << " plan=" << mPlan;
	if (mHaveOctet3a) {
		os << " presentation=" << mPresentationIndicator;
		os << " screening=" << mScreeningIndicator;
	}
	os << " digits=" << mDigits;
}



// vim: ts=4 sw=4


