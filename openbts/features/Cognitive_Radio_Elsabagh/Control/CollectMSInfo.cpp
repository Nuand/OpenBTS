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


#include <cstdio>

#include <vector>

#include "GSMLogicalChannel.h"
#include "GSML3RRMessages.h" // mobID
#include "Sockets.h"

#include "CollectMSInfo.h"
#include "RRLPQueryController.h"

namespace GSM {
namespace RRLP {

// Assistance Data:
/*
15> rrlp:decodePDU(util:zeroones_to_bin("010000100000010000001000")).           {ok,{'PDU',2,         
        {msrPositionRsp,
            {'MsrPosition-Rsp',asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
                asn1_NOVALUE,asn1_NOVALUE,
                {'LocationError',notEnoughSats,asn1_NOVALUE},
                asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}}}}
*/

/** @return true if got RR response
*/
PositionResult doRRLPQuery(L3MobileIdentity mobID, LogicalChannel* chan, BitVector& rrlp_position_request)
{
    return RRLPQueryManager::instance()->doTransaction(mobID, chan, rrlp_position_request);
}

PositionResult doRRLPQuery(L3MobileIdentity mobID, LogicalChannel* chan)
{
    return RRLPQueryManager::instance()->doTransaction(mobID, chan);
}

// Not actually used right now.
void doMultipleRRLPQueries(L3MobileIdentity mobID, LogicalChannel* chan)
{
	if (doRRLPQuery(mobID, chan).mValid) {
		int tests = 0;
		bool done = false;
		while (tests > 0 && !done) {
			LOG(INFO) << "test " << tests;
			done = doRRLPQuery(mobID, chan).mValid;
			--tests;
		}
	}
}

void logMSInfo(LogicalChannel* LCH, L3MobileIdentity mobID)
{
	// Log without position, in a way that we can easily tell there was no position - use negatives (we only
	// use positives right now
	PositionResult pr;
	pr.mValid = true;
	pr.mPos.mLat = pr.mPos.mLon = -1;
	logMSInfo(LCH, pr, mobID);
}


// FIXME -- Most of this should be replaced with Wireshark GSMTAP logging.

// mobID is just logged as INFO for debugging
void logMSInfo(LogicalChannel* LCH, const PositionResult& pr, L3MobileIdentity mobID)
{
    // Config says we should collect. So collect.
	Timeval now;
	std::ostringstream stream;
	stream << now;
	// GPS (if valid)
	if (pr.mValid) {
		stream << "," << pr.mPos.mLat << "," << pr.mPos.mLon;
	} else {
		stream << ",0.0,0.0";
	}
	// own transceiver parameters
	stream << "," << LCH->FER() << "," << LCH->RSSI() << "," << LCH->timingError() << ","
		   << LCH->actualMSPower() << "," << LCH->actualMSTiming();
	// MS transceiver parameters
	const L3MeasurementResults& m = LCH->SACCH()->measurementResults();
	unsigned RXLEV_NCELL[6]; m.RXLEV_NCELL(RXLEV_NCELL);
	unsigned BCCH_FREQ_NCELL[6]; m.BCCH_FREQ_NCELL(BCCH_FREQ_NCELL);
	unsigned BSIC_NCELL[6]; m.BSIC_NCELL(BSIC_NCELL);
	stream << "," << m.BA_USED() << "," << m.DTX_USED() << "," << m.MEAS_VALID()
		   << "," << m.RXLEV_FULL_SERVING_CELL()
		   << "," << m.RXLEV_SUB_SERVING_CELL()
		   << "," << m.RXQUAL_FULL_SERVING_CELL()
		   << "," << m.RXQUAL_SUB_SERVING_CELL()
		   << "," << m.NO_NCELL();
	for (unsigned i = 0 ; i < m.NO_NCELL() && i < 6; ++i) {
	   stream << "," << RXLEV_NCELL[i] << "," << BCCH_FREQ_NCELL[i]
			  << "," << BSIC_NCELL[i];
	}
	unsigned port = gConfig.getNum("Indications.TargetPort");
	const char* ip = gConfig.getStr("Indications.TargetIP");
	UDPSocket collect_sock(0, ip, port);
	collect_sock.write(stream.str().c_str());
	LOG(INFO) << "(" << mobID << ")Lat,Lon,FER,RSSI,timingError,actualMSPower,actualMSTiming";
	LOG(INFO) << stream.str().c_str();
}

void collectMSInfo(L3MobileIdentity mobID, LogicalChannel* LCH, bool withRRLP)
{
	// Check configuration
	// RRLP Test code. we now have a channel, send a request for position
	static const char* rrlpOnName = "GSM.RRLP";
	PositionResult pr;
	if (gConfig.defines(rrlpOnName) && gConfig.getNum(rrlpOnName) == 1 && withRRLP) {
		LOG(INFO) << "Doing RRLPQuery";
		pr = doRRLPQuery(mobID, LCH); // FIXME: put this back in RRLP::RRLPQuery
		logMSInfo(LCH, pr, mobID);
	} else {
		logMSInfo(LCH, mobID);
	}
}

}; // namespace RRLP
}; // namespace GSM

/*
 Replies I got for the RRLP so far:

 Mostly:
  0000 0101 00011011
  0    5    1B
       MM   MM TMSI REALLOCATION COMPLETE

  This is an MM TMSI REALLOCATION COMPLETE

 Sometimes:
  000001100001001001100001
  0000 0110 00010010 01100001
  0    6    12       61
       RR   RR Status Message type non existent or not implemented.

  This is an RR message (good!)

  --- Android G1 19.8
  0000 0110 00111000 00000000 00000001 00000110
  0000 0110 00111000 00000000 00001110 0100001000000100100110010001001011010000000000010000000000010010100100000100010000000000001011000101000000100100


  Once:

  0000 0110 00111000 00000000 00010010
  0    RR   

  001 0 001 0 00000 1
  Ref num = 1
  No extensions
  msrPositionRsp

    The rest (didn't parse this yet)
0010011001000110101101000000000000000000000000111100101100100001000000010000000100001010100010101000110000001100000011101000111010

<PDU>
    <referenceNumber>1</referenceNumber>
    <component>
        <msrPositionRsp>
            <locationError>
                <locErrorReason><gpsAssDataMissing/></locErrorReason>
                <additionalAssistanceData>
                    <gpsAssistanceData>68 00 00 07 96 42 02 02 15 15 18 18 1D 1D</gpsAssistanceData>
                </additionalAssistanceData>
            </locationError>
        </msrPositionRsp>
    </component>
</PDU>

 
  Also got (not sure from who - lots of phones here, I think they camped without me really requiring that - maybe should add an option to ignore specific IMSI's?).

 0000 0110 00110100 11011110 01010011
      RR   GPRS Suspension Request
 
 111110010001010100010011000000000001010000010111100101100000000100000001


 And
00000110 00111000 00000000 00010010 010000100000010010011001000110101101000000000000000000000000111100101100100001000000010000000100001010100010101000110000001100000011101000111010

*/


