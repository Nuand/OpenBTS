/**@file
	@brief Radio Resource messages, GSM 04.08 9.1.
*/

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

#include <iterator> // for L3APDUData::text

#include "GSML3RRElements.h"

#include <Logger.h>



using namespace std;
using namespace GSM;




void L3CellOptionsBCCH::writeV(L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp,0,1);
	dest.writeField(wp,mPWRC,1);
	dest.writeField(wp,mDTX,2);
	dest.writeField(wp,mRADIO_LINK_TIMEOUT,4);
}



void L3CellOptionsBCCH::text(ostream& os) const
{
	os << "PWRC=" << mPWRC;
	os << " DTX=" << mDTX;
	os << " RADIO_LINK_TIMEOUT=" << mRADIO_LINK_TIMEOUT;
}



void L3CellOptionsSACCH::writeV(L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp,(mDTX>>2)&0x01,1);
	dest.writeField(wp,mPWRC,1);
	dest.writeField(wp,mDTX&0x03,2);
	dest.writeField(wp,mRADIO_LINK_TIMEOUT,4);
}



void L3CellOptionsSACCH::text(ostream& os) const
{
	os << "PWRC=" << mPWRC;
	os << " DTX=" << mDTX;
	os << " RADIO_LINK_TIMEOUT=" << mRADIO_LINK_TIMEOUT;
}



void L3CellSelectionParameters::writeV(L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp,mCELL_RESELECT_HYSTERESIS,3);
	dest.writeField(wp,mMS_TXPWR_MAX_CCH,5);
	dest.writeField(wp,mACS,1);
	dest.writeField(wp,mNECI,1);
	dest.writeField(wp,mRXLEV_ACCESS_MIN,6);
}




void L3CellSelectionParameters::text(ostream& os) const
{
	os << "CELL-RESELECT-HYSTERESIS=" << mCELL_RESELECT_HYSTERESIS;
	os << " MS-TXPWR-MAX-CCH=" << mMS_TXPWR_MAX_CCH;
	os << " ACS=" << mACS;
	os << " NECI=" << mNECI;
	os << " RXLEV-ACCESS-MIN=" << mRXLEV_ACCESS_MIN;
}







void L3ControlChannelDescription::writeV(L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp,0,1);
	dest.writeField(wp,mATT,1);
	dest.writeField(wp,mBS_AG_BLKS_RES,3);
	dest.writeField(wp,mCCCH_CONF,3);
	dest.writeField(wp,0,5);
	dest.writeField(wp,mBS_PA_MFRMS,3);
	dest.writeField(wp,mT3212,8);
}



void L3ControlChannelDescription::text(ostream& os) const
{
	os << "ATT=" << mATT;
	os << " BS_AG_BLKS_RES=" << mBS_AG_BLKS_RES;
	os << " CCCH_CONF=" << mCCCH_CONF;
	os << " BS_PA_MFRMS=" << mBS_PA_MFRMS;
	os << " T3212=" << mT3212;
}


bool L3FrequencyList::contains(unsigned wARFCN) const
{
	for (unsigned i=0; i<mARFCNs.size(); i++) {
		if (mARFCNs[i]==wARFCN) return true;
	}
	return false;
}


unsigned L3FrequencyList::base() const
{
	if (mARFCNs.size()==0) return 0;
	unsigned retVal = mARFCNs[0];
	for (unsigned i=1; i<mARFCNs.size(); i++) {
		unsigned thisVal = mARFCNs[i];
		if (thisVal<retVal) retVal=thisVal;
	}
	return retVal;
}


unsigned L3FrequencyList::spread() const
{
	if (mARFCNs.size()==0) return 0;
	unsigned max = mARFCNs[0];
	for (unsigned i=0; i<mARFCNs.size(); i++) {
		if (mARFCNs[i]>max) max=mARFCNs[i];
	}
	return max - base();
}


void L3FrequencyList::writeV(L3Frame& dest, size_t &wp) const
{
	// If this were used as Frequency List, it had to be coded
	// as the variable bit map format, GSM 04.08 10.5.2.13.7.
	// But it is used as Cell Channel Description and is coded
	// as the variable bit map format, GSM 04.08 10.5.2.1b.7.
	// Difference is in abscence of Length field.
	
	// The header occupies first 7 most significant bits of
	// the first V-part octet and should be 1000111b=0x47 for
	// the variable length bitmap.
	dest.writeField(wp,0x47,7);

	// base ARFCN
	unsigned baseARFCN = base();
	dest.writeField(wp,baseARFCN,10);
	// bit map
	unsigned delta = spread();
	unsigned numBits = 8*lengthV() - 17;
	if (numBits<delta) { LOG(ALARM) << "L3FrequencyList cannot encode full ARFCN set"; }
	for (unsigned i=0; i<numBits; i++) {
		unsigned thisARFCN = baseARFCN + 1 + i;
		if (contains(thisARFCN)) dest.writeField(wp,1,1);
		else dest.writeField(wp,0,1);
	}
}



void L3FrequencyList::text(ostream& os) const
{
	int size = mARFCNs.size();
	for (int i=0; i<size; i++) {
		os << mARFCNs[i] << " ";
	}
}




void L3CellChannelDescription::writeV(L3Frame& dest, size_t& wp) const
{
	dest.fillField(wp,0,3);
	L3FrequencyList::writeV(dest,wp);
}



void L3NeighborCellsDescription::writeV(L3Frame& dest, size_t& wp) const
{
	dest.fillField(wp,0,3);
	L3FrequencyList::writeV(dest,wp);
}



void L3NeighborCellsDescription::text(ostream& os) const
{
	os << "EXT-IND=0 BA-IND=0 ";
	os << " ARFCNs=(";
	L3FrequencyList::text(os);
	os << ")";
}




void L3NCCPermitted::writeV(L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp,mPermitted,8);
}



void L3NCCPermitted::text(ostream& os) const
{
	os << hex << "0x" << mPermitted << dec;
}




void L3RACHControlParameters::writeV(L3Frame& dest, size_t &wp) const
{
	// GMS 04.08 10.5.2.29
	dest.writeField(wp, mMaxRetrans, 2);
	dest.writeField(wp, mTxInteger, 4);
	dest.writeField(wp, mCellBarAccess, 1);
	dest.writeField(wp, mRE, 1);
	dest.writeField(wp, mAC, 16);
}


void L3RACHControlParameters::text(ostream& os) const
{
	os << "maxRetrans=" << mMaxRetrans;
	os << " txInteger=" << mTxInteger;
	os << " cellBarAccess=" << mCellBarAccess;
	os << " RE=" << mRE;
	os << hex << " AC=0x" << mAC << dec;
}



void L3PageMode::writeV(L3Frame& dest, size_t &wp) const
{
	// PageMode is 1/2 octet. Spare[3:2], PM[1:0]
	dest.writeField(wp, 0x00, 2);
	dest.writeField(wp, mPageMode, 2);
}

void L3PageMode::parseV( const L3Frame &src, size_t &rp)
{
	// Read out spare bits.
	rp += 2;
	// Read out PageMode.
	mPageMode = src.readField(rp, 2);
} 



void L3PageMode::text(ostream& os) const
{
	os << mPageMode;
}


void L3DedicatedModeOrTBF::writeV( L3Frame& dest, size_t &wp )const
{
	// 1/2 Octet. 
	dest.writeField(wp, 0, 1);
	dest.writeField(wp, mTMA, 1);
	dest.writeField(wp, mDownlink, 1);
	dest.writeField(wp, mDMOrTBF, 1);
}


void L3DedicatedModeOrTBF::text(ostream& os) const
{
	os << "TMA=" << mTMA;
	os << " Downlink=" << mDownlink;
	os << " DMOrTBF=" << mDMOrTBF;
}


void L3ChannelDescription::writeV( L3Frame &dest, size_t &wp ) const 
{
	// GSM 04.08 10.5.2.5
// 					Channel Description Format (non-hopping)
// 		 	7      6      5      4      3     2      1      0
//	  [         TSC       ][ H=0 ][ SPARE(0,0)][ ARFCN[9:8] ]  Octet 3
//	  [                ARFCN[7:0]                           ]  Octet 4 H=0
//

	// HACK -- Hard code for non-hopping.
	assert(mHFlag==0);
	dest.writeField(wp,mTypeAndOffset,5);
	dest.writeField(wp,mTN,3);
	dest.writeField(wp,mTSC,3);
	dest.writeField(wp,0,3);				// H=0 + 2 spares
	dest.writeField(wp,mARFCN,10);
}



void L3ChannelDescription::parseV(const L3Frame& src, size_t &rp)
{
	// GSM 04.08 10.5.2.5
	mTypeAndOffset = (TypeAndOffset)src.readField(rp,5);
	mTN = src.readField(rp,3);
	mTSC = src.readField(rp,3);
	mHFlag = src.readField(rp,1);
	if (mHFlag) {
		mMAIO = src.readField(rp,6);
		mHSN = src.readField(rp,6);
	} else {
		rp += 2;	// skip 2 spare bits
		mARFCN = src.readField(rp,10);
	}
}


void L3ChannelDescription::text(std::ostream& os) const
{

	os << "typeAndOffset=" << mTypeAndOffset;
	os << " TN=" << mTN;
	os << " TSC=" << mTSC;
	os << " ARFCN=" << mARFCN;	
}



void L3RequestReference::writeV( L3Frame &dest, size_t &wp ) const 
{


// 					Request Reference Format.
// 		 	7      6      5      4      3     2      1      0
//	  [      			RequestReference [7:0]   			]  Octet 2
//	  [			T1[4:0]                 ][   T3[5:3]        ]  Octet 3
//	  [       T3[2:0]     ][            T2[4:0]             ]  Octet 4

	dest.writeField(wp, mRA, 8);
	dest.writeField(wp, mT1p, 5);
	dest.writeField(wp, mT3, 6);
	dest.writeField(wp, mT2, 5);
}


void L3RequestReference::text(ostream& os) const
{
	os << "RA=" << mRA;	
	os << " T1'=" << mT1p;
	os << " T2=" << mT2;
	os << " T3=" << mT3;	
}




void L3TimingAdvance::writeV( L3Frame &dest, size_t &wp ) const
{
	dest.writeField(wp, 0x00, 2);
	dest.writeField(wp, mTimingAdvance, 6);
}

void L3TimingAdvance::text(ostream& os) const
{
    os << mTimingAdvance;
}



void L3RRCause::writeV( L3Frame &dest, size_t &wp ) const
{
	dest.writeField(wp, mCauseValue, 8);	
}

void L3RRCause::parseV( const L3Frame &src, size_t &rp )
{
	mCauseValue = src.readField(rp, 8);
}

void L3RRCause::text(ostream& os) const
{
	os << "0x" << hex << mCauseValue << dec;
}




void L3PowerCommand::writeV( L3Frame &dest, size_t &wp )const
{
	dest.writeField(wp, 0, 3);
	dest.writeField(wp, mCommand, 5);
}


void L3PowerCommand::text(ostream& os) const
{
	os << mCommand;
}





void L3ChannelMode::writeV( L3Frame& dest, size_t &wp) const
{
	dest.writeField(wp, mMode, 8);
}

void L3ChannelMode::parseV(const L3Frame& src, size_t& rp)
{
	mMode = (Mode)src.readField(rp,8);
}



ostream& GSM::operator<<(ostream& os, L3ChannelMode::Mode mode)
{
	switch (mode) {
		case L3ChannelMode::SignallingOnly: os << "signalling"; break;
		case L3ChannelMode::SpeechV1: os << "speech1"; break;
		case L3ChannelMode::SpeechV2: os << "speech2"; break;
		case L3ChannelMode::SpeechV3: os << "speech3"; break;
		default: os << "?" << (int)mode << "?";
	}
	return os;
}

void L3ChannelMode::text(ostream& os) const
{
	os << mMode;
}

// Application Information IEs

// APDU ID

void L3APDUID::writeV( L3Frame& dest, size_t &wp) const
{
	// APDU ID is 1/2 octet. Protocol Identifier [3:0]
    dest.writeField(wp,mProtocolIdentifier,4);
}

void L3APDUID::parseV( const L3Frame &src, size_t &rp)
{
	// Read out Protocol Identifier.
	mProtocolIdentifier = src.readField(rp, 4);
}

void L3APDUID::text(ostream& os) const
{
	os << mProtocolIdentifier;
}

// APDU Flags

void L3APDUFlags::writeV( L3Frame& dest, size_t &wp) const
{
	// APDU Flags is 1/2 octet. Protocol Identifier [3:0]
    dest.writeField(wp,0,1); // spare
    dest.writeField(wp,mCR,1); // C/R
    dest.writeField(wp,mFirstSegment,1);
    dest.writeField(wp,mLastSegment,1);
}

void L3APDUFlags::parseV( const L3Frame &src, size_t &rp)
{
	// Read out Protocol Identifier.
    rp += 1; // skip spare
	mCR = src.readField(rp, 1);
	mFirstSegment = src.readField(rp, 1);
	mLastSegment = src.readField(rp, 1);
}

void L3APDUFlags::text(ostream& os) const
{
	os << mCR << "," << mFirstSegment << "," << mLastSegment;
}

// APDU Data

L3APDUData::~L3APDUData()
{
}

L3APDUData::L3APDUData()
    :L3ProtocolElement()
{
}

L3APDUData::L3APDUData(BitVector data)
    :L3ProtocolElement()
    ,mData(data)
{
}



void L3APDUData::writeV( L3Frame& dest, size_t &wp) const
{
    // we only need to write the data part
    // TODO - single line please. copy / memcpy, anything better then a for loop
    LOG(DEBUG) << "L3APDUData: writeV " << mData.size() << " bits";
    mData.copyToSegment(dest, wp);
    wp += mData.size() / 8;
}

void L3APDUData::parseV( const L3Frame& src, size_t &rp, size_t expectedLength )
{
    LOG(DEBUG) << "L3APDUData: parseV " << expectedLength << " bytes";
    mData.resize(expectedLength*8);
    src.copyToSegment(mData, rp, expectedLength*8); // expectedLength is bytes, not bits
    //for ( size_t i = 0 ; i < expectedLength ; ++i)
    //    mData[i] = src.readField(rp, 8);
}

void L3APDUData::text(ostream& os) const
{
    // TODO - use the following two lines (get rid of the "char / char*" error)
    //std::ostream_iterator<std::string> output( os, "" );
    //std::copy( mData.begin(), mData.end(), output );
    for (size_t i = 0 ; i < mData.size() ; ++i)
        os << mData[i];
}


void L3MeasurementResults::parseV(const L3Frame& frame, size_t &rp)
{
	// GSM 04.08 10.5.2.20
	mBA_USED = frame.readField(rp,1);
	mDTX_USED = frame.readField(rp,1);
	mRXLEV_FULL_SERVING_CELL = frame.readField(rp,6);
	rp++;	// spare
	mMEAS_VALID = frame.readField(rp,1);
	mRXLEV_SUB_SERVING_CELL = frame.readField(rp,6);
	rp++;	// spare
	mRXQUAL_FULL_SERVING_CELL = frame.readField(rp,3);
	mRXQUAL_SUB_SERVING_CELL = frame.readField(rp,3);
	mNO_NCELL = frame.readField(rp,3);
	for (unsigned i=0; i<6; i++) {
		mRXLEV_NCELL[i] = frame.readField(rp,6);
		mBCCH_FREQ_NCELL[i] = frame.readField(rp,5);
		mBSIC_NCELL[i] = frame.readField(rp,6);
	}
}


void L3MeasurementResults::text(ostream& os) const
{
	// GSM 04.08 10.5.2.20
	os << "BA_USED=" << mBA_USED;
	os << " DTX_USED=" << mDTX_USED;
	os << " MEAS_VALID=" << mMEAS_VALID;
	// Note that the value of the MEAS-VALID bit is reversed
	// from what you might expect.
	if (mMEAS_VALID) return;
	os << " RXLEV_FULL_SERVING_CELL=" << mRXLEV_FULL_SERVING_CELL;
	os << " RXLEV_SUB_SERVING_CELL=" << mRXLEV_SUB_SERVING_CELL;
	os << " RXQUAL_FULL_SERVING_CELL=" << mRXQUAL_FULL_SERVING_CELL;
	os << " RXQUAL_SUB_SERVING_CELL=" << mRXQUAL_SUB_SERVING_CELL;
	os << " NO_NCELL=" << mNO_NCELL;
	// no measurements?
	if (mNO_NCELL==0) return;
	// no neighbor list?
	if (mNO_NCELL==7) return;
	for (unsigned i=0; i<mNO_NCELL; i++) {
		os << " RXLEV_NCELL" << i+1 << "=" << mRXLEV_NCELL[i];
		os << " BCCH_FREQ_NCELL" << i+1 << "=" << mBCCH_FREQ_NCELL[i];
		os << " BSIC_NCELL" << i+1 << "=" << mBSIC_NCELL[i];
	}
}


unsigned L3MeasurementResults::RXLEV_NCELL(unsigned * target) const
{
	for (unsigned i=0; i<mNO_NCELL; i++) target[i] = mRXLEV_NCELL[i];
	return mNO_NCELL;
}


unsigned L3MeasurementResults::BCCH_FREQ_NCELL(unsigned * target) const
{
	for (unsigned i=0; i<mNO_NCELL; i++) target[i] = mBCCH_FREQ_NCELL[i];
	return mNO_NCELL;
}


unsigned L3MeasurementResults::BSIC_NCELL(unsigned * target) const
{
	for (unsigned i=0; i<mNO_NCELL; i++) target[i] = mBSIC_NCELL[i];
	return mNO_NCELL;
}


int L3MeasurementResults::decodeLevToDBm(unsigned lev) const
{
	// See GSM 05.08 8.1.4.
	// SCALE is 0 for anything but the ENHANCED MEASUREMENT REPORT message.
	return -111 + lev;
}

float L3MeasurementResults::decodeQualToBER(unsigned qual) const
{
	// See GSM 05.08 8.2.4.
	// Convert lowest value as "0" instead of 0.14%.
	static const float vals[] = {0.0, 0.28, 0.57, 1.13, 2.26, 4.53, 9.05, 18.10};
	assert(qual<8);
	return 0.01*vals[qual];
}







// vim: ts=4 sw=4
