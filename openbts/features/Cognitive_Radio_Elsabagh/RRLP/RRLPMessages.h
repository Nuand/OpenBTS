/*
* Copyright 2008 Free Software Foundation, Inc.
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


/*
	Implementation of the LCS RRLP Message. We just support sending a
    request for GPS computer MS based position, and parsing the response
    is not yet done, just dumping it out.
*/




#ifndef RRLP_MESSAGE_H
#define RRLP_MESSAGE_H

#include <GSMLogicalChannel.h>
#include <GSML3Message.h>
#include <GSML3CCElements.h>
#include <GSML3MMElements.h>


namespace RRLP {


class RRLPReadError : public GSM::GSMError {
	public:
	RRLPReadError():GSMError() {}
};
#define RRLP_READ_ERROR {throw RRLPReadError();}

// Forward declarations
class RRLPComponent;
class RRLPMeasurementResponse;

// High level interface

/**
	generates a single APDU with a RRLP having a MsrPositionRequest component
	for a GPS MS Based location computation.
	@param chan The DCCH on which o run the transaction
	@return true on success
*/
// FIXME -- Once the parser works, this can return RRLPMeasurementResponse* or NULL on failure.
bool RRLPQuery(GSM::LogicalChannel *);


//@} // RRLP Message

/**@name A Single RRLP PDU */
//@{

/** GSM 04.31 9.2 */
class RRLPMessage {

	protected:

	unsigned mReferenceNumber;
    RRLPComponent*  mComponent;

	public:

	RRLPMessage(unsigned referenceNumber, RRLPComponent* component)
		:mReferenceNumber(referenceNumber), mComponent(component)
	{}

	virtual ~RRLPMessage() {}

	virtual void parse( const GSM::L3Frame& frame );

	virtual void write( GSM::L3Frame& frame ) const;

	virtual void text(std::ostream& os) const;

};

std::ostream& operator<<(std::ostream& os, const RRLPMessage& msg);


}; // namespace RRLP

#endif

// vim: ts=4 sw=4
