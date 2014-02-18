/*
* Copyright 2008 Free Software Foundation, Inc.
* Copyright 2011 Range Networks, Inc.
*
* This software is distributed under multiple licenses;
* see the COPYING file in the main directory for licensing
* information for this specific distribuion.
*
* This use of this software may be subject to additional restrictions.
* See the LEGAL file in the main directory for details.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*/


#include "GSMCommon.h"

using namespace GSM;
using namespace std;


ostream& GSM::operator<<(ostream& os, L3PD val)
{
	switch (val) {
		case L3CallControlPD: os << "Call Control"; break;
		case L3MobilityManagementPD: os << "Mobility Management"; break;
		case L3RadioResourcePD: os << "Radio Resource"; break;
		default: os << hex << "0x" << (int)val << dec;
	}
	return os;
}


unsigned char GSM::encodeGSMChar(unsigned char ascii)
{
	// Given an ASCII char, return the corresponding GSM char.
	// Do it with a lookup table, generated on the first call.
	// You might be tempted to replace this init with some more clever NULL-pointer trick.
	// -- Don't.  This is thread-safe.
	static char reverseTable[256]={'?'};
	static volatile bool init = false;
	if (!init) {
		for (size_t i=0; i<sizeof(gGSMAlphabet); i++) {
			reverseTable[(unsigned)gGSMAlphabet[i]]=i;
		}
		// Set the flag last to be thread-safe.
		init=true;
	}
	return reverseTable[(unsigned)ascii];
}


char GSM::encodeBCDChar(char ascii)
{
	// Given an ASCII char, return the corresponding BCD.
	if ((ascii>='0') && (ascii<='9')) return ascii-'0';
	switch (ascii) {
		case '.': return 11;
		case '*': return 11;
		case '#': return 12;
		case 'a': return 13;
		case 'b': return 14;
		case 'c': return 15;
		default: return 15;
	}
}




ostream& GSM::operator<<(ostream& os, TypeOfNumber type)
{
	switch (type) {
		case UnknownTypeOfNumber: os << "unknown"; break;
		case InternationalNumber: os << "international"; break;
		case NationalNumber: os << "national"; break;
		case NetworkSpecificNumber: os << "network-specific"; break;
		case ShortCodeNumber: os << "short code"; break;
		default: os << "?" << (int)type << "?";
	}
	return os;
}


ostream& GSM::operator<<(ostream& os, NumberingPlan plan)
{
	switch (plan) {
		case UnknownPlan: os << "unknown"; break;
		case E164Plan: os << "E.164/ISDN"; break;
		case X121Plan: os << "X.121/data"; break;
		case F69Plan: os << "F.69/Telex"; break;
		case NationalPlan: os << "national"; break;
		case PrivatePlan: os << "private"; break;
		default: os << "?" << (int)plan << "?";
	}
	return os;
}

ostream& GSM::operator<<(ostream& os, MobileIDType wID)
{
	switch (wID) {
		case NoIDType: os << "None"; break;
		case IMSIType: os << "IMSI"; break;
		case IMEIType: os << "IMEI"; break;
		case TMSIType: os << "TMSI"; break;
		case IMEISVType: os << "IMEISV"; break;
		default: os << "?" << (int)wID << "?";
	}
	return os;
}




bool Z100Timer::expired() const
{
	assert(mLimitTime!=0);
	// A non-active timer does not expire.
	if (!mActive) return false;
	return mEndTime.passed();
}

void Z100Timer::set()
{
	assert(mLimitTime!=0);
	mEndTime = Timeval(mLimitTime);
	mActive=true;
} 

void Z100Timer::expire()
{
	mEndTime = Timeval(0);
	mActive=true;
} 


void Z100Timer::set(long wLimitTime)
{
	mLimitTime = wLimitTime;
	set();
} 


long Z100Timer::remaining() const
{
	if (!mActive) return 0;
	long rem = mEndTime.remaining();
	if (rem<0) rem=0;
	return rem;
}

void Z100Timer::wait() const
{
	while (!expired()) msleep(remaining());
}

// vim: ts=4 sw=4
