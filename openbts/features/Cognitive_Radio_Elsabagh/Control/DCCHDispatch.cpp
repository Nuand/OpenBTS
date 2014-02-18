/**@file Idle-mode dispatcher for dedicated control channels. */

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




#include "ControlCommon.h"
#include <GSMLogicalChannel.h>
#include <GSML3MMMessages.h>
#include <GSML3RRMessages.h>
#include <SIPUtility.h>
#include <SIPInterface.h>

using namespace std;
using namespace GSM;
using namespace Control;




/**
	Dispatch the appropriate controller for a Mobility Management message.
	@param req A pointer to the initial message.
	@param DCCH A pointer to the logical channel for the transaction.
*/
void DCCHDispatchMM(const L3MMMessage* req, LogicalChannel *DCCH)
{
	assert(req);
	L3MMMessage::MessageType MTI = (L3MMMessage::MessageType)req->MTI();
	switch (MTI) {
		case L3MMMessage::LocationUpdatingRequest:
			LocationUpdatingController(dynamic_cast<const L3LocationUpdatingRequest*>(req),
									dynamic_cast<SDCCHLogicalChannel*>(DCCH));
			break;
		case L3MMMessage::IMSIDetachIndication:
			IMSIDetachController(dynamic_cast<const L3IMSIDetachIndication*>(req),
									dynamic_cast<SDCCHLogicalChannel*>(DCCH));
			break;
		case L3MMMessage::CMServiceRequest:
			CMServiceResponder(dynamic_cast<const L3CMServiceRequest*>(req),DCCH);
			break;
		default:
			LOG(NOTICE) << "unhandled MM message " << MTI << " on " << DCCH->type();
			throw UnsupportedMessage();
	}
}


/**
	Dispatch the appropriate controller for a Radio Resource message.
	@param req A pointer to the initial message.
	@param DCCH A pointer to the logical channel for the transaction.
*/
void DCCHDispatchRR(const L3RRMessage* req, LogicalChannel *DCCH)
{
	LOG(DEBUG) << "checking MTI"<< (L3RRMessage::MessageType)req->MTI();

	assert(req);
	L3RRMessage::MessageType MTI = (L3RRMessage::MessageType)req->MTI();
	switch (MTI) {
		case L3RRMessage::PagingResponse:
			PagingResponseHandler(dynamic_cast<const L3PagingResponse*>(req),DCCH);
			break;
		case L3RRMessage::AssignmentComplete:
			AssignmentCompleteHandler(dynamic_cast<const L3AssignmentComplete*>(req),
										dynamic_cast<TCHFACCHLogicalChannel*>(DCCH));
			break;
		default:
			LOG(NOTICE) << "unhandled RR message " << MTI << " on " << DCCH->type();
			throw UnsupportedMessage();
	}
}






/** Example of a closed-loop, persistent-thread control function for the DCCH. */
void Control::DCCHDispatcher(LogicalChannel *DCCH)
{
	while (1) {
		try {
			// Wait for a transaction to start.
			LOG(DEBUG) << "waiting for " << DCCH->type() << " ESTABLISH";
			waitForPrimitive(DCCH,ESTABLISH);
			// Pull the first message and dispatch a new transaction.
			const L3Message *message = getMessage(DCCH);
			LOG(DEBUG) << "received " << *message;
			// Each protocol has it's own sub-dispatcher.
			switch (message->PD()) {
				case L3MobilityManagementPD:
					DCCHDispatchMM(dynamic_cast<const L3MMMessage*>(message),DCCH);
					break;
				case L3RadioResourcePD:
					DCCHDispatchRR(dynamic_cast<const L3RRMessage*>(message),DCCH);
					break;
				default:
					LOG(NOTICE) << "unhandled protocol " << message->PD() << " on " << DCCH->type();
					throw UnsupportedMessage();
			}
			delete message;
		}

		// Catch the various error cases.

		catch (ChannelReadTimeout except) {
			clearTransactionHistory(except.transactionID());
			LOG(NOTICE) << "ChannelReadTimeout";
			// Cause 0x03 means "abnormal release, timer expired".
			DCCH->send(L3ChannelRelease(0x03));
		}
		catch (UnexpectedPrimitive except) {
			clearTransactionHistory(except.transactionID());
			LOG(NOTICE) << "UnexpectedPrimitive";
			// Cause 0x62 means "message type not not compatible with protocol state".
			DCCH->send(L3ChannelRelease(0x62));
		}
		catch (UnexpectedMessage except) {
			clearTransactionHistory(except.transactionID());
			LOG(NOTICE) << "UnexpectedMessage";
			// Cause 0x62 means "message type not not compatible with protocol state".
			DCCH->send(L3ChannelRelease(0x62));
		}
		catch (UnsupportedMessage except) {
			clearTransactionHistory(except.transactionID());
			LOG(NOTICE) << "UnsupportedMessage";
			// Cause 0x61 means "message type not implemented".
			DCCH->send(L3ChannelRelease(0x61));
		}
		catch (Q931TimerExpired except) {
			clearTransactionHistory(except.transactionID());
			LOG(NOTICE) << "Q.931 T3xx timer expired";
			// Cause 0x03 means "abnormal release, timer expired".
			DCCH->send(L3ChannelRelease(0x03));
		}
		catch (SIP::SIPTimeout) {
			LOG(WARN) << "Uncaught SIPTimeout, will leave a stray transcation";
			// Cause 0x03 means "abnormal release, timer expired".
			DCCH->send(L3ChannelRelease(0x03));
		}
		catch (SIP::SIPError) {
			LOG(WARN) << "Uncaught SIPError, will leave a stray transcation";
			// Cause 0x01 means "abnormal release, unspecified".
			DCCH->send(L3ChannelRelease(0x01));
		}

		//FIXME -- What's the GSM 04.08 Txxx value for this?
		// Probably N200 * T200
		//LOG(DEBUG) << "DCCHDisptacher waiting for " << DCCH->type() << " RELEASE";
		//waitForPrimitive(DCCH,RELEASE,20000);
	}
}




// vim: ts=4 sw=4
