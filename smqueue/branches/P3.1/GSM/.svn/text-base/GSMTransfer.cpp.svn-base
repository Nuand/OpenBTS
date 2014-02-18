/*
* Copyright 2008 Free Software Foundation, Inc.
*
* This software is distributed under multiple licenses; see the COPYING file in the main directory for licensing information for this specific distribuion.
*
* This use of this software may be subject to additional restrictions.
* See the LEGAL file in the main directory for details.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*/




#include <iostream>

#include "GSMTransfer.h"
#include "GSML3Message.h"


using namespace std;
using namespace GSM;



ostream& GSM::operator<<(ostream& os, const L3Frame& frame)
{
	os << "primitive=" << frame.primitive();
	os << " raw=(";
	frame.hex(os);
	os << ")";
	return os;
}



ostream& GSM::operator<<(ostream& os, Primitive prim)
{
	switch (prim) {
		case ESTABLISH: os << "ESTABLISH"; break;
		case RELEASE: os << "RELEASE"; break;
		case DATA: os << "DATA"; break;
		case UNIT_DATA: os << "UNIT_DATA"; break;
		case ERROR: os << "ERROR"; break;
		case HARDRELEASE: os << "HARDRELEASE"; break;
		default: os << "?" << (int)prim << "?";
	}
	return os;
}




L3Frame::L3Frame(const L3Message& msg, Primitive wPrimitive)
	:BitVector(msg.bitsNeeded()),mPrimitive(wPrimitive),
	mL2Length(msg.L2Length())
{
	msg.write(*this);
}



L3Frame::L3Frame(const char* hexString)
	:mPrimitive(DATA)
{
	size_t len = strlen(hexString);
	mL2Length = len/2;
	resize(len*4);
	size_t wp=0;
	for (size_t i=0; i<len; i++) {
		char c = hexString[i];
		int v = c - '0';
		if (v>9) v = c - 'a' + 10;
		writeField(wp,v,4);
	}
}


L3Frame::L3Frame(const char* binary, size_t len)
	:mPrimitive(DATA)
{
	mL2Length = len;
	resize(len*8);
	size_t wp=0;
	for (size_t i=0; i<len; i++) {
		writeField(wp,binary[i],8);
	}
}



static const unsigned fillPattern[8] = {0,0,1,0,1,0,1,1};

void L3Frame::writeH(size_t &wp)
{
	unsigned fillBit = fillPattern[wp%8];
	writeField(wp,!fillBit,1);
}


void L3Frame::writeL(size_t &wp)
{
	unsigned fillBit = fillPattern[wp%8];
	writeField(wp,fillBit,1);
}



// vim: ts=4 sw=4
