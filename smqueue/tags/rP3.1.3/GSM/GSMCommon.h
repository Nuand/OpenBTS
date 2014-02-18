/**@file Common-use GSM declarations, most from the GSM 04.xx and 05.xx series. */
/*
* Copyright 2008, 2009, 2010 Free Software Foundation, Inc.
* Copyright 2010 Kestrel Signal Processing, Inc.
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



#ifndef GSMCOMMON_H
#define GSMCOMMON_H

#include <stdlib.h>
#include <sys/time.h>
#include <ostream>
#include <vector>

#include <Threads.h>
#include <Timeval.h>
#include <BitVector.h>




namespace GSM {

/**@namespace GSM This namespace covers L1 FEC, L2 and L3 message translation. */



/** A base class for GSM exceptions. */
class GSMError {};


enum GSMAlphabet {
	ALPHABET_7BIT,
	ALPHABET_8BIT,
	ALPHABET_UCS2
};

/**@name Support for GSM 7-bit alphabet, GSM 03.38 6.2.1. */
//@{
/**
	Indexed by GSM 7-bit, returns ISO-8859-1.
	We do not support the extended table, so 0x1B is a space.
	FIXME -- ISO-8859-1 doesn't support Greek!
*/
static const unsigned char gGSMAlphabet[] = "@\243$\245\350\351\371\354\362\347\n\330\370\r\305\345D_FGLOPCSTZ \306\346\337\311 !\"#\244%&\'()*+,-./0123456789:;<=>?\241ABCDEFGHIJKLMNOPQRSTUVWXYZ\304\326\321\334\247\277abcdefghijklmnopqrstuvwxyz\344\366\361\374\341";
unsigned char encodeGSMChar(unsigned char ascii);
inline unsigned char decodeGSMChar(unsigned char sms) { return gGSMAlphabet[(unsigned)sms]; }
//@}


/**@name BCD-ASCII mapping, GMS 04.08 Table 10.5.118. */
//@{
/** Indexed by BCD, returns ASCII. */
static const char gBCDAlphabet[] = "0123456789.#abc";
char encodeBCDChar(char ascii);
inline char decodeBCDChar(char bcd) { return gBCDAlphabet[(unsigned)bcd]; }
//@}





/** GSM 04.08 Table 10.5.118 and GSM 03.40 9.1.2.5 */
enum TypeOfNumber {
	UnknownTypeOfNumber = 0,
	InternationalNumber = 1,
	NationalNumber = 2,
	NetworkSpecificNumber = 3,
	ShortCodeNumber = 4,
	AlphanumericNumber = 5,
	AbbreviatedNumber = 6
};

std::ostream& operator<<(std::ostream&, TypeOfNumber);


/** GSM 04.08 Table 10.5.118 and GSM 03.40 9.1.2.5 */
enum NumberingPlan {
	UnknownPlan = 0,
	E164Plan = 1,
	X121Plan = 3,
	F69Plan = 4,
	NationalPlan = 8,
	PrivatePlan = 9,
	ERMESPlan = 10
};

std::ostream& operator<<(std::ostream&, NumberingPlan);



/** Mobile identity types, GSM 04.08 10.5.1.4 */
enum MobileIDType {
	NoIDType = 0,
	IMSIType = 1,
	IMEIType = 2,
	IMEISVType = 3,
	TMSIType = 4
};

std::ostream& operator<<(std::ostream& os, MobileIDType);




/**
 L3 Protocol Discriminator, GSM 04.08 10.2, GSM 04.07 11.2.3.1.1.
*/
enum L3PD {
	L3GroupCallControlPD=0x00,
	L3BroadcastCallControlPD=0x01,
	L3PDSS1PD=0x02,
	L3CallControlPD=0x03,
	L3PDSS2PD=0x04,
	L3MobilityManagementPD=0x05,
	L3RadioResourcePD=0x06,
	L3MobilityManagementGPRSPD=0x08,
	L3SMSPD=0x09,
	L3GPRSSessionManagementPD=0x0a,
	L3NonCallSSPD=0x0b,
	L3LocationPD=0x0c,
	L3ExtendedPD=0x0e,
	L3TestProcedurePD=0x0f,
	L3UndefinedPD=-1
};



std::ostream& operator<<(std::ostream& os, L3PD val);




/**
	CCITT Z.100 activity timer, as described in GSM 04.06 5.1.
	All times are in milliseconds.
*/
class Z100Timer {

	private:

	Timeval mEndTime;		///< the time at which this timer will expire
	long mLimitTime;		///< timeout in milliseconds
	bool mActive;			///< true if timer is active

	public:

	/** Create a timer with a given timeout in milliseconds. */
	Z100Timer(long wLimitTime)
		:mLimitTime(wLimitTime),
		mActive(false)
	{}

	/** Blank constructor; if you use this object, it will assert. */
	Z100Timer():mLimitTime(0),mActive(false) {}

	/** True if the timer is active and expired. */
	bool expired() const;

	/** Force the timer into an expired state. */
	void expire();

	/** Start or restart the timer. */
	void set();

	/** Start or restart the timer, possibly specifying a new limit. */
	void set(long wLimitTime);

	/** Stop the timer. */
	void reset() { assert(mLimitTime!=0); mActive = false; }

	/** Returns true if the timer is active. */
	bool active() const { return mActive; }

	/**
		Remaining time until expiration, in milliseconds.
		Returns zero if the timer has expired.
	*/
	long remaining() const;

	/**
		Block until the timer expires.
		Returns immediately if the timer is not running.
	*/
	void wait() const;
};





}; 	// namespace GSM


#endif

// vim: ts=4 sw=4
