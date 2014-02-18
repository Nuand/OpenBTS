/*
* Copyright 2008, 2009 Free Software Foundation, Inc.
*

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

* This software is distributed under the terms of the GNU Affero Public License.
* See the COPYING file in the main directory for details.
*
* This use of this software may be subject to additional restrictions.
* See the LEGAL file in the main directory for details.
*/

#include <cstdio>
#include <vector>

#include "RRLPMessages.h"
#include <GSML3RRMessages.h>
#include <Logger.h>

using namespace std;
using namespace GSM;
using namespace RRLP;

////////////////////////////////////////////////////////////////////////////////
// High Level Interface

bool RRLP::RRLPQuery(GSM::LogicalChannel* chan)
{
    //static const char init_request_msbased_gps[4] = {'\x40', '\x01', '\x78', '\x98'};
    static const char init_request_msbased_gps[4] = {'\x40', '\x01', '\x78', '\xA8'};
    static vector<char> request_msbased_gps(init_request_msbased_gps,
        init_request_msbased_gps + sizeof(init_request_msbased_gps));
    /*
    Request a GPS MS Based position in maximum 2 seconds with accuracy 60, a single result
    expected.

    PER:

    rrr e ccc e ooooo O mm aaaaaaa pp mmm u ___
    010 0 000 0 00000 0 01 0111100 01 001 1 000
4017898

    XER:

<PDU>
    <referenceNumber>2</referenceNumber>
    <component>
        <msrPositionReq>
            <positionInstruct>
                <methodType>
                    <msBased>60</msBased>
                </methodType>
                <positionMethod><gps/></positionMethod>
                <measureResponseTime>1</measureResponseTime>
                <useMultipleSets><oneSet/></useMultipleSets>
            </positionInstruct>
        </msrPositionReq>
    </component>
</PDU>

    */
    LOG(INFO) << "RRLPQuery about to be sent";
    chan->send(L3ApplicationInformation(request_msbased_gps));
	// Receive an L3 frame with a timeout.  Timeout is RRLP max + 1 second.
    LOG(INFO) << "RRLPQuery sent";
	L3Frame* resp = chan->recv(10000);
    LOG(INFO) << "RRLPQuery recv returned";
	if (!resp) {
		LOG(INFO) << "RRLPQuery timed out";
		return false;
	}

	// FIXME -- Actually parse the response.
	if (resp) { LOG(INFO) << "RRLPQuery returned " << *resp; }
	delete resp;
	return true;
}

////////////////////////////////////////////////////////////////////////////////
// RRLPMessage implementation

// write


// parse
//    uper_encode(&asn_DEF_PDU, rrlp, write_out, stdout);


void RRLPMessage::parse( const GSM::L3Frame& frame ) {
    // TODO - read the APDU, Then just dump it for the moment.
    // not using asn1c (to quote someone, "Yikes!!!")

}

void RRLPMessage::write( GSM::L3Frame& frame ) const
{
    // TODO - write the PDU_t using the uper encoder, and then
    // write into the frame using the BitVector API
}

void RRLPMessage::text(std::ostream& os) const
{
    os << "RefNum=(" << mReferenceNumber << ")\n";
    // TODO - call the xer print or something.
    //os << "Component=(" << mComponent << ")\n";
}



// vim: ts=4 sw=4
