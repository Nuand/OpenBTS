/**@file GSM/SIP Call Control -- GSM 04.08, ISDN ITU-T Q.931, SIP IETF RFC-3261, RTP IETF RFC-3550. */
/*
* Copyright 2008, 2009, 2010 Free Software Foundation, Inc.
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
	Abbreviations:
	MTC -- Mobile Terminated Connect (someone calling the mobile)
	MOC -- Mobile Originated Connect (mobile calling out)
	MTD -- Mobile Terminated Disconnect (other party hangs up)
	MOD -- Mobile Originated Disconnect (mobile hangs up)
	E-MOC -- Emergency Mobile Originated Connect (mobile calling out)
*/


#include <Globals.h>

#include "ControlCommon.h"

#include <GSMLogicalChannel.h>
#include <GSML3RRMessages.h>
#include <GSML3MMMessages.h>
#include <GSML3CCMessages.h>
#include <GSMConfig.h>

#include <SIPInterface.h>
#include <SIPUtility.h>
#include <SIPMessage.h>
#include <SIPEngine.h>

#include <Logger.h>

using namespace std;
using namespace GSM;
using namespace Control;
using namespace SIP;






/**
	Return an even UDP port number for the RTP even/odd pair.
*/
unsigned allocateRTPPorts()
{
	// FIXME -- We need a real port allocator.
	const unsigned base = gConfig.getNum("RTP.Start");
	const unsigned range = gConfig.getNum("RTP.Range");
	const unsigned top = base+range;
	static Mutex lock;
	// Pick a random starting point.
	static unsigned port = base + 2*(random()%(range/2));
	lock.lock();
	unsigned retVal = port;
	port += 2;
	if (port>=top) port=base;
	lock.unlock();
	return retVal;
}



/**
	Force clearing on the GSM side.
	@param transaction The call transaction record.
	@param LCH The logical channel.
	@param cause The L3 abort cause.
*/
void forceGSMClearing(TransactionEntry& transaction, LogicalChannel *LCH, const L3Cause& cause)
{
	LOG(INFO) << "Q.931 state " << transaction.Q931State();
	if (transaction.Q931State()==TransactionEntry::NullState) return;
	if (!transaction.clearing()) {
		LCH->send(L3Disconnect(1-transaction.TIFlag(),transaction.TIValue(),cause));
	}
	LCH->send(L3ReleaseComplete(1-transaction.TIFlag(),transaction.TIValue()));
	LCH->send(L3ChannelRelease());
	transaction.resetTimers();
	transaction.Q931State(TransactionEntry::NullState);
	LCH->send(RELEASE);
	gTransactionTable.update(transaction);
}


/**
	Force clearing on the SIP side.
	@param transaction The call transaction record.
*/
void forceSIPClearing(TransactionEntry& transaction)
{
	LOG(INFO) << "SIP state " << transaction.SIP().state();
	if (transaction.SIP().state()==SIP::Cleared) return;
	if (transaction.SIP().state()!=SIP::Clearing) {
		// This also changes the SIP state to "clearing".
		transaction.SIP().MODSendBYE();
	} else {
		transaction.SIP().MODResendBYE();
	}
	transaction.SIP().MODWaitForOK();
	gTransactionTable.update(transaction);
}



/**
	Abort the call.  Does not clear the transaction history.
	@param transaction The call transaction record.
	@param LCH The logical channel.
	@param cause The L3 abort cause.
*/
void abortCall(TransactionEntry& transaction, LogicalChannel *LCH, const L3Cause& cause)
{
	LOG(INFO) << "cause: " << cause << ", transction: " << transaction;
	forceGSMClearing(transaction,LCH,cause);
	forceSIPClearing(transaction);
	clearTransactionHistory(transaction);
}




/**
	Allocate a TCH and clean up any failure.
	@param SDCCH The SDCCH that will be used to send the assignment.
	@return A pointer to the TCH or NULL on failure.
*/
TCHFACCHLogicalChannel *allocateTCH(SDCCHLogicalChannel *SDCCH)
{
	TCHFACCHLogicalChannel *TCH = gBTS.getTCH();
	if (!TCH) {
		LOG(NOTICE) << "CONGESTION, no TCH available for assignment";
		// Cause 0x16 is "congestion".
		SDCCH->send(L3CMServiceReject(0x16));
		SDCCH->send(L3ChannelRelease());
	}
	return TCH;
}





/**
	Assign a full rate traffic channel and clean up any failures.
	@param SDCCH The SDCCH on which to send the assignment.
	@param TCH The TCH to be assigned.
	@bool True on successful transfer.
*/
bool assignTCHF(TransactionEntry& transaction, SDCCHLogicalChannel *SDCCH, TCHFACCHLogicalChannel *TCH)
{
	TCH->open();
	TCH->setPhy(*SDCCH);

	// Send the assignment.
	LOG(INFO) << "assignTCHF sending AssignmentCommand for " << TCH << " on " << SDCCH;
	SDCCH->send(L3AssignmentCommand(TCH->channelDescription(),L3ChannelMode(L3ChannelMode::SpeechV1)));

	// This read is SUPPOSED to time out if the assignment was successful.
	// Pad the timeout just in case there's a large latency somewhere.
	L3Frame *result = SDCCH->recv(T3107ms+2000);
	if (result==NULL) {
		LOG(INFO) << "assignmentTCHF exiting normally";
		SDCCH->send(HARDRELEASE);
		return true;
	}

	// If we got here, the assignment failed.
	LOG(NOTICE) << "assignTCHF received " << *result;
	delete result;

	// Turn off the TCH.
	TCH->send(RELEASE);
	// RR Cause 0x04 -- "abnormal release, no activity on the radio path"
	SDCCH->send(L3ChannelRelease(0x04));
	// Shut down the SIP side of the call.
	forceSIPClearing(transaction);
	// Clean up the transaction table.
	clearTransactionHistory(transaction);
	// Indicate failure.
	return false;
}




/**
	Process a message received from the phone during a call.
	This function processes all deviations from the "call connected" state.
	For now, we handle call clearing and politely reject everything else.
	@param transaction The transaction record for this call.
	@param LCH The logical channel for the transaction.
	@param message A pointer to the receiver message.
	@return true If the call has been cleared and the channel released.
*/
bool callManagementDispatchGSM(TransactionEntry& transaction, LogicalChannel* LCH, const L3Message *message)
{
	LOG(DEBUG) << "from " << transaction.subscriber() << " message " << *message;

	// FIXME -- This dispatch section should be something more efficient with PD and MTI swtiches.

	// Actually check state before taking action.
	//if (transaction.SIP().state()==SIP::Cleared) return true;
	//if (transaction.Q931State()==TransactionEntry::NullState) return true;

	// Call connection steps.

	// Connect Acknowledge
	if (dynamic_cast<const L3ConnectAcknowledge*>(message)) {
		LOG(INFO) << "GSM Connect Acknowledge " << transaction.subscriber();
		transaction.resetTimers();
		transaction.Q931State(TransactionEntry::Active);
		gTransactionTable.update(transaction);
		return false;
	}

	// Connect
	// GSM 04.08 5.2.2.5 and 5.2.2.6
	if (dynamic_cast<const L3Connect*>(message)) {
		LOG(INFO) << "GSM Connect " << transaction.subscriber();
		transaction.resetTimers();
		transaction.Q931State(TransactionEntry::Active);
		gTransactionTable.update(transaction);
		return false;
	}

	// Call Confirmed
	// GSM 04.08 5.2.2.3.2
	// "Call Confirmed" is the GSM MTC counterpart to "Call Proceeding"
	if (dynamic_cast<const L3CallConfirmed*>(message)) {
		LOG(INFO) << "GSM Call Confirmed " << transaction.subscriber();
		transaction.T303().reset();
		transaction.T310().set();
		transaction.Q931State(TransactionEntry::MTCConfirmed);
		gTransactionTable.update(transaction);
		return false;
	}

	// Alerting
	// GSM 04.08 5.2.2.3.2
	if (dynamic_cast<const L3Alerting*>(message)) {
		LOG(INFO) << "GSM Alerting " << transaction.subscriber();
		transaction.T310().reset();
		transaction.T301().set();
		transaction.Q931State(TransactionEntry::CallReceived);
		gTransactionTable.update(transaction);
		return false;
	}

	// Call clearing steps.
	// Good diagrams in GSM 04.08 7.3.4

	// FIXME -- We should be checking TI values against the transaction object.

	// Disconnect (1st step of MOD)
	// GSM 04.08 5.4.3.2
	if (dynamic_cast<const L3Disconnect*>(message)) {
		LOG(INFO) << "GSM Disconnect " << transaction.subscriber();
		transaction.resetTimers();
		LCH->send(L3Release(1-transaction.TIFlag(),transaction.TIValue()));
		transaction.T308().set();
		transaction.Q931State(TransactionEntry::ReleaseRequest);
		transaction.SIP().MODSendBYE();
		gTransactionTable.update(transaction);
		return false;
	}

	// Release (2nd step of MTD)
	if (dynamic_cast<const L3Release*>(message)) {
		LOG(INFO) << "GSM Release " << transaction.subscriber();
		transaction.resetTimers();
		LCH->send(L3ReleaseComplete(1-transaction.TIFlag(),transaction.TIValue()));
		LCH->send(L3ChannelRelease());
		transaction.Q931State(TransactionEntry::NullState);
		transaction.SIP().MTDSendOK();
		gTransactionTable.update(transaction);
		return true;
	}

	// Release Complete (3nd step of MOD)
	// GSM 04.08 5.4.3.4
	if (dynamic_cast<const L3ReleaseComplete*>(message)) {
		LOG(INFO) << "GSM Release Complete " << transaction.subscriber();
		transaction.resetTimers();
		LCH->send(L3ChannelRelease());
		transaction.Q931State(TransactionEntry::NullState);
		transaction.SIP().MODWaitForOK();
		clearTransactionHistory(transaction);
		return true;
	}

	// IMSI Detach -- the phone is shutting off.
	if (const L3IMSIDetachIndication* detach = dynamic_cast<const L3IMSIDetachIndication*>(message)) {
		// The IMSI detach procedure will release the LCH.
		LOG(INFO) << "GSM IMSI Detach " << transaction.subscriber();
		IMSIDetachController(detach,LCH);
		forceSIPClearing(transaction);
		clearTransactionHistory(transaction);
		return true;
	}

	// Start DTMF
	// Send a SIP INFO to generate a tone in Asterisk.
	if (const L3StartDTMF* keypress = dynamic_cast<const L3StartDTMF*>(message)) {
		unsigned keyVal = encodeBCDChar(keypress->key().IA5());
		LOG(INFO) << "DMTF key=" << keyVal <<  ' ' << transaction.subscriber();
		bool success = transaction.SIP().sendINFOAndWaitForOK(keyVal);
		// Cause 0x3f means "service or option not available".
		if (success) LCH->send(L3StartDTMFAcknowledge(1-transaction.TIFlag(),transaction.TIValue(),keypress->key()));
		else LCH->send(L3StartDTMFReject(1-transaction.TIFlag(),transaction.TIValue(),0x3f));
		return false;
	}

	// Stop DTMF
	// Since we use SIP INFO we just ack.
	if (dynamic_cast<const L3StopDTMF*>(message)) {
		LCH->send(L3StopDTMFAcknowledge(1-transaction.TIFlag(),transaction.TIValue()));
		return false;
	}

	// Stubs for unsupported features.
	// We need to answer the handset so it doesn't hang.

	// CM Service Request
	// This is the gateway to a much more complex state machine.
	// For now, we're cutting it off right here.
	if (dynamic_cast<const L3CMServiceRequest*>(message)) {
		LOG(NOTICE) << "cannot accept additional CM Service Request from " << transaction.subscriber();
		// Cause 0x20 means "serivce not supported".
		LCH->send(L3CMServiceReject(0x20));
		return false;
	}

	// Hold
	if (dynamic_cast<const L3Hold*>(message)) {
		LOG(NOTICE) << "rejecting hold request from " << transaction.subscriber();
		// Default cause is 0x3f, option not available
		LCH->send(L3HoldReject(1-transaction.TIFlag(),transaction.TIValue()));
		return false;
	}

	if (message) LOG(NOTICE) << "no support for message " << *message << " from " << transaction.subscriber();
	else LOG(NOTICE) << "no support for unrecognized message from " << transaction.subscriber();


	// If we got here, we're ignoring the message.
	return false;
}






/**
	Update vocoder data transfers in both directions.
	@param transaction The transaction object for this call.
	@param TCH The traffic channel for this call.
	@return True if anything was transferred.
*/
bool updateCallTraffic(TransactionEntry &transaction, TCHFACCHLogicalChannel *TCH)
{
	bool activity = false;

	SIPEngine& engine = transaction.SIP();


	// Transfer in the downlink direction (RTP->GSM).
	// Blocking call.  On average returns 1 time per 20 ms.
	// Returns non-zero if anything really happened.
	// Make the rxFrame buffer big enough for G.711.
	unsigned char rxFrame[160];
	if (engine.RxFrame(rxFrame)) {
		activity = true;
		TCH->sendTCH(rxFrame);
	}

	// Transfer in the uplink direction (GSM->RTP).
	// Flush FIFO to limit latency.
	unsigned maxQ = gConfig.getNum("GSM.MaxSpeechLatency");
	while (TCH->queueSize()>maxQ) delete[] TCH->recvTCH();
	if (unsigned char *txFrame = TCH->recvTCH()) {
		activity = true;
		// Send on RTP.
		engine.TxFrame(txFrame);
		delete[] txFrame;
	}

	// Return a flag so the caller will know if anything transferred.
	return activity;
}




/**
	Check GSM signalling.
	Can block for up to 52 GSM L1 frames (240 ms) because LCH::send is blocking.
	@param transaction The call's TransactionEntry.
	@param LCH The call's logical channel (TCH/FACCH or SDCCH).
	@return true If the call was cleared.
*/
bool updateGSMSignalling(TransactionEntry &transaction, LogicalChannel *LCH, unsigned timeout=0)
{
	if (transaction.Q931State()==TransactionEntry::NullState) return true;

	// Any Q.931 timer expired?
	if (transaction.timerExpired()) {
		// Cause 0x66, "recover on timer expiry"
		abortCall(transaction,LCH,L3Cause(0x66));
		return true;
	}

	// Look for a control message from MS side.
	if (L3Frame *l3 = LCH->recv(timeout)) {
		// Check for lower-layer error.
		if (l3->primitive() == ERROR) return true;
		// Parse and dispatch.
		L3Message *l3msg = parseL3(*l3);
		delete l3;
		bool cleared = false;
		if (l3msg) {
			LOG(DEBUG) << "received " << *l3msg;
			cleared = callManagementDispatchGSM(transaction, LCH, l3msg);
			delete l3msg;
		}
		return cleared;
	}

	// If we are here, we have timed out, but assume the call is still running.
	return false;
}



/**
	Check SIP and GSM signalling.
	Can block for up to 52 GSM L1 frames (240 ms) because LCH::send is blocking.
	@param transaction The call's TransactionEntry.
	@param LCH The call's logical channel (TCH/FACCH or SDCCH).
	@return true If the call is cleared in both domains.
*/
bool updateSignalling(TransactionEntry &transaction, LogicalChannel *LCH, unsigned timeout=0)
{

	bool GSMCleared = (updateGSMSignalling(transaction,LCH,timeout));

	// Look for a SIP message.
	SIPEngine& engine = transaction.SIP();
	if (engine.MTDCheckBYE() == SIP::Clearing) {
		if (!transaction.clearing()) {
			LOG(DEBUG) << "got BYE";
			LCH->send(L3Disconnect(1-transaction.TIFlag(),transaction.TIValue()));
			transaction.T305().set();
			transaction.Q931State(TransactionEntry::DisconnectIndication);
			// Return false, because it the call is not yet cleared.
			return false;
		} else {
			// If we're already clearing, send BYE again.
			//engine.MODSendBYE();
		}
	}
	bool SIPCleared = (engine.state()==SIP::Cleared);

	return GSMCleared && SIPCleared;
}




/**
	Poll for activity while in a call.
	Sleep if needed to prevent fast spinning.
	Will block for up to 250 ms.
	@param transaction The call's TransactionEntry.
	@param TCH The call's TCH+FACCH.
	@return true If the call was cleared.
*/
bool pollInCall(TransactionEntry &transaction, TCHFACCHLogicalChannel *TCH)
{
	// See if the radio link disappeared.
	if (TCH->radioFailure()) {
		LOG(NOTICE) << "radio link failure, dropped call";
		forceSIPClearing(transaction);
		clearTransactionHistory(transaction);
		return true;
	}
	// Process pending SIP and GSM signalling.
	// If this returns true, it means the call is fully cleared.
	if (updateSignalling(transaction,TCH)) return true;
	// Transfer vocoder data.
	// If anything happened, then the call is still up.
	if (updateCallTraffic(transaction,TCH)) return false;
	// If nothing happened, sleep so we don't burn up the CPU cycles.
	msleep(250);
	return false;
}


/**
	Pause for a given time while managing the connection.
	Returns on timeout or call clearing.
	Used for debugging to simulate ringing at terminating end.
	@param transaction The transaction record for the call.
	@param TCH The TCH+FACCH sed for this call.
	@param waitTime_ms The maximum time to wait, in ms.
	@return true If the call is cleared during the wait.
*/
bool waitInCall(TransactionEntry& transaction, TCHFACCHLogicalChannel *TCH, unsigned waitTime_ms)
{
	Timeval targetTime(waitTime_ms);
	LOG(DEBUG);
	while (!targetTime.passed()) {
		if (pollInCall(transaction,TCH)) return true;
	}
	return false;
}



/**
	This is the standard call manangement loop, regardless of the origination type.
	This function returns when the call is cleared and the channel is released.
	@param transaction The transaction record for this call, will be cleared on exit.
	@param TCH The TCH+FACCH for the call.
*/
void callManagementLoop(TransactionEntry &transaction, TCHFACCHLogicalChannel* TCH)
{
	LOG(INFO) << transaction.subscriber() << " call connected";
	transaction.SIP().FlushRTP();
	// poll everything until the call is cleared
	while (!pollInCall(transaction,TCH)) { }
	clearTransactionHistory(transaction);
}




/**
	This function starts MOC on the SDCCH to the point of TCH assignment. 
	@param req The CM Service Request that started all of this.
	@param LCH The logical used to initiate call setup.
*/
void Control::MOCStarter(const L3CMServiceRequest* req, LogicalChannel *LCH)
{
	assert(LCH);
	assert(req);
	LOG(INFO) << *req;

	// Determine if very early assignment already happened.
	bool veryEarly = (LCH->type()==FACCHType);

	// If we got a TMSI, find the IMSI.
	// Note that this is a copy, not a reference.
	L3MobileIdentity mobileIdentity = req->mobileIdentity();
	resolveIMSI(mobileIdentity,LCH);


	// FIXME -- At this point, verify the that subscriber has access to this service.
	// If the subscriber isn't authorized, send a CM Service Reject with
	// cause code, 0x41, "requested service option not subscribed",
	// followed by a Channel Release with cause code 0x6f, "unspecified".
	// Otherwise, proceed to the next section of code.
	// For now, we are assuming that the phone won't make a call if it didn't
	// get registered.

	// Allocate a TCH for the call, if we don't have it already.
	TCHFACCHLogicalChannel *TCH = NULL;
	if (!veryEarly) {
		TCH = allocateTCH(dynamic_cast<SDCCHLogicalChannel*>(LCH));
		// It's OK to just return on failure; allocateTCH cleaned up already,
		// and the SIP side and transaction record don't exist yet.
		if (TCH==NULL) return;
	}

	// Let the phone know we're going ahead with the transaction.
	LOG(DEBUG) << "sending CMServiceAccept";
	LCH->send(L3CMServiceAccept());

	// Get the Setup message.
	// GSM 04.08 5.2.1.2
	L3Message* msg_setup = getMessage(LCH);
	const L3Setup *setup = dynamic_cast<const L3Setup*>(msg_setup);
	if (!setup) {
		if (msg_setup) {
			LOG(WARN) << "Unexpected message " << *msg_setup;
			delete msg_setup;
		}
		throw UnexpectedMessage();
	}
	LOG(INFO) << *setup;
	// Pull out the L3 short transaction information now.
	// See GSM 04.07 11.2.3.1.3.
	unsigned L3TI = setup->TIValue();
	if (!setup->haveCalledPartyBCDNumber()) {
		// FIXME -- This is quick-and-dirty, not following GSM 04.08 5.
		LOG(WARN) << "MOC setup with no number";
		// Cause 0x60 "Invalid mandatory information"
		LCH->send(L3ReleaseComplete(1,L3TI,L3Cause(0x60)));
		LCH->send(L3ChannelRelease());
		// The SIP side and transaction record don't exist yet.
		// So we're done.
		delete msg_setup;
		return;
	}

	LOG(DEBUG) << "SIP start engine";
	// Get the users sip_uri by pulling out the IMSI.
	const char *IMSI = mobileIdentity.digits();
	// Pull out Number user is trying to call and use as the sip_uri.
	const char *bcd_digits = setup->calledPartyBCDNumber().digits();

	// Create a transaction table entry so the TCH controller knows what to do later.
	// The transaction on the TCH is a continuation of this one and uses the same ID.
	TransactionEntry transaction(mobileIdentity,
		req->serviceType(),
		L3TI,
		setup->calledPartyBCDNumber());
	assert(transaction.TIFlag()==0);
	transaction.SIP().User(IMSI);
	transaction.Q931State(TransactionEntry::MOCInitiated);
	LCH->transactionID(transaction.ID());
	if (!veryEarly) TCH->transactionID(transaction.ID());
	LOG(DEBUG) << "transaction: " << transaction;
	gTransactionTable.add(transaction);

	// At this point, we have enough information start the SIP call setup.
	// We also have a SIP side and a transaction that will need to be
	// cleaned up on abort or clearing.

	// Now start a call by contacting asterisk.
	// Engine methods will return their current state.	
	// The remote party will start ringing soon.
	LOG(DEBUG) << "starting SIP (INVITE) Calling "<<bcd_digits;
	unsigned basePort = allocateRTPPorts();
	SIPState state = transaction.SIP().MOCSendINVITE(bcd_digits,gConfig.getStr("SIP.IP"),basePort,SIP::RTPGSM610);
	LOG(DEBUG) << "SIP state="<<state;
	LOG(DEBUG) << "Q.931 state=" << transaction.Q931State();

	// Once we can start SIP call setup, send Call Proceeding.
	LOG(DEBUG) << "Sending Call Proceeding";
	LCH->send(L3CallProceeding(1,L3TI));
	transaction.Q931State(TransactionEntry::MOCProceeding);
	gTransactionTable.update(transaction);
	// Finally done with the Setup message.
	delete msg_setup;

	// The transaction is moving on to the MOCController.
	// If we need a TCH assignment, we do it here.
	gTransactionTable.update(transaction);
	LOG(DEBUG) << "transaction: " << transaction;
	if (veryEarly) {
		// For very early assignment, we need a mode change.
		static const L3ChannelMode mode(L3ChannelMode::SpeechV1);
		LCH->send(L3ChannelModeModify(LCH->channelDescription(),mode));
		L3Message *msg_ack = getMessage(LCH);
		const L3ChannelModeModifyAcknowledge *ack =
			dynamic_cast<L3ChannelModeModifyAcknowledge*>(msg_ack);
		if (!ack) {
			if (msg_ack) {
				LOG(WARN) << "Unexpected message " << *msg_ack;
				delete msg_setup;
			}
			throw UnexpectedMessage(transaction.ID());
		}
		// Cause 0x06 is "channel unacceptable"
		bool modeOK = (ack->mode()==mode);
		delete msg_ack;
		if (!modeOK) return abortCall(transaction,LCH,L3Cause(0x06));
		MOCController(transaction,dynamic_cast<TCHFACCHLogicalChannel*>(LCH));
	} else {
		// For late assignment, send the TCH assignment now.
		// This dispatcher on the next channel will continue the transaction.
		assignTCHF(transaction,dynamic_cast<SDCCHLogicalChannel*>(LCH),TCH);
	}
}





/**
	Continue MOC process on the TCH.
	@param transaction The call state and SIP interface.
	@param TCH The traffic channel to be used.
*/
void Control::MOCController(TransactionEntry& transaction, TCHFACCHLogicalChannel* TCH)
{
	LOG(INFO) << "transaction: " << transaction;
	unsigned L3TI = transaction.TIValue();
	assert(transaction.TIFlag()==0);
	assert(TCH);


	// Look for RINGING or OK from the SIP side.
	// There's a T310 running on the phone now.
	// The phone will initiate clearing if it expires.
	while (transaction.Q931State()!=TransactionEntry::CallReceived) {

		if (updateGSMSignalling(transaction,TCH)) return;
		if (transaction.clearing()) return abortCall(transaction,TCH,L3Cause(0x7F));

		LOG(INFO) << "MOC A: wait for Ringing or OK";
		SIPState state = transaction.SIP().MOCWaitForOK();
		LOG(DEBUG) << "MOC A: SIP state="<<state;
		switch (state) {
			case SIP::Busy:
				LOG(INFO) << "MOC A: SIP:Busy, abort";
				return abortCall(transaction,TCH,L3Cause(0x11));
			case SIP::Fail:
				LOG(NOTICE) << "MOC A: SIP:Fail, abort";
				return abortCall(transaction,TCH,L3Cause(0x7F));
			case SIP::Ringing:
				LOG(INFO) << "MOC A: SIP:Ringing, send Alerting and move on";
				TCH->send(L3Alerting(1,L3TI));
				transaction.Q931State(TransactionEntry::CallReceived);
				break;
			case SIP::Active:
				LOG(DEBUG) << "MOC A: SIP:Active, move on";
				transaction.Q931State(TransactionEntry::CallReceived);
				break;
			case SIP::Proceeding:
				LOG(DEBUG) << "MOC A: SIP:Proceeding, send progress";
				TCH->send(L3Progress(1,L3TI));
				break;
			case SIP::Timeout:
				LOG(NOTICE) << "MOC A: SIP:Timeout, reinvite";
				state = transaction.SIP().MOCResendINVITE();
				break;
			default:
				LOG(NOTICE) << "MOC A: SIP unexpected state " << state;
				break;
		}
	}
	gTransactionTable.update(transaction);

	// There's a question here of what entity is generating the "patterns"
	// (ringing, busy signal, etc.) during call set-up.  For now, we're ignoring 
	// that question and hoping the phone will make its own ringing pattern.


	// Wait for the SIP session to start.
	// There's a timer on the phone that will initiate clearing if it expires.
	LOG(INFO) << "wait for SIP OKAY";
	SIPState state = transaction.SIP().state();
	while (state!=SIP::Active) {

		LOG(DEBUG) << "wait for SIP session start";
		state = transaction.SIP().MOCWaitForOK();
		LOG(DEBUG) << "SIP state "<< state;

		// check GSM state
		if (updateGSMSignalling(transaction,TCH)) return;
		if (transaction.clearing()) return abortCall(transaction,TCH,L3Cause(0x7F));

		// parse out SIP state
		switch (state) {
			case SIP::Busy:
				// Should this be possible at this point?
				LOG(INFO) << "MOC B: SIP:Busy, abort";
				return abortCall(transaction,TCH,L3Cause(0x11));
			case SIP::Fail:
				LOG(INFO) << "MOC B: SIP:Fail, abort";
				return abortCall(transaction,TCH,L3Cause(0x7F));
			case SIP::Proceeding:
				LOG(DEBUG) << "MOC B: SIP:Proceeding, NOT sending progress";
				//TCH->send(L3Progress(1,L3TI));
				break;
			// For these cases, do nothing.
			case SIP::Timeout:
				// FIXME We should abort if this happens too often.
				// For now, we are relying on the phone, which may have bugs of its own.
			case SIP::Active:
			default:
				break;
		}
	} 
	gTransactionTable.update(transaction);
	
	// Let the phone know the call is connected.
	LOG(INFO) << "sending Connect to handset";
	TCH->send(L3Connect(1,L3TI));
	transaction.T313().set();
	transaction.Q931State(TransactionEntry::ConnectIndication);
	gTransactionTable.update(transaction);

	// The call is open.
	transaction.SIP().MOCInitRTP();
	transaction.SIP().MOCSendACK();

	// Get the Connect Acknowledge message.
	while (transaction.Q931State()!=TransactionEntry::Active) {
		LOG(DEBUG) << "MOC Q.931 state=" << transaction.Q931State();
		if (updateGSMSignalling(transaction,TCH,T313ms)) return abortCall(transaction,TCH,L3Cause(0x7F));
	}

	// At this point, everything is ready to run the call.
	gTransactionTable.update(transaction);
	callManagementLoop(transaction,TCH);

	// The radio link should have been cleared with the call.
	// So just return.
}




void Control::MTCStarter(TransactionEntry& transaction, LogicalChannel *LCH)
{
	assert(LCH);
	LOG(INFO) << "MTC on " << LCH->type() << " transaction: "<< transaction;

	// Determine if very early assigment already happened.
	bool veryEarly = false;
	if (LCH->type()==FACCHType) veryEarly=true;

	// Allocate a TCH for the call.
	TCHFACCHLogicalChannel *TCH = NULL;
	if (!veryEarly) {
		TCH = allocateTCH(dynamic_cast<SDCCHLogicalChannel*>(LCH));
		// It's OK to just return on failure; allocateTCH cleaned up already.
		// The orphaned transaction will be cleared automatically later.
		if (TCH==NULL) return;
	}


	// Get transaction identifiers.
	// This transaction was created by the SIPInterface when it
	// processed the INVITE that started this call.
	if (!veryEarly) TCH->transactionID(transaction.ID());	
	LCH->transactionID(transaction.ID());	
	unsigned L3TI = transaction.TIValue();
	assert(transaction.TIFlag()==1);

	// GSM 04.08 5.2.2.1
	LOG(INFO) << "sending GSM Setup to call " << transaction.calling();
	LCH->send(L3Setup(0,L3TI,L3CallingPartyBCDNumber(transaction.calling())));
	transaction.T303().set();
	transaction.Q931State(TransactionEntry::CallPresent);
	gTransactionTable.update(transaction);

	// Wait for Call Confirmed message.
	LOG(DEBUG) << "wait for GSM Call Confirmed";
	while (transaction.Q931State()!=TransactionEntry::MTCConfirmed) {
		if (transaction.SIP().MTCSendTrying()==SIP::Fail) {
			LOG(NOTICE) << "call failed on SIP side";
			LCH->send(RELEASE);
			// Cause 0x03 is "no route to destination"
			return abortCall(transaction,LCH,L3Cause(0x03));
		}
		// FIXME -- What's the proper timeout here?
		// It's the SIP TRYING timeout, whatever that is.
		if (updateGSMSignalling(transaction,LCH,1000)) {
			LOG(INFO) << "Release from GSM side";
			LCH->send(RELEASE);
			return;
		}
		// Check for SIP cancel, too.
		if (transaction.SIP().MTCWaitForACK()==SIP::Fail) {
			LOG(NOTICE) << "call failed on SIP side";
			LCH->send(RELEASE);
			// Cause 0x10 is "normal clearing"
			return abortCall(transaction,LCH,L3Cause(0x10));
		}
	}

	// The transaction is moving to the MTCController.
	// Once this update happens, don't change the transaction object again in this function.
	gTransactionTable.update(transaction);
	LOG(DEBUG) << "transaction: " << transaction;
	if (veryEarly) {
		// For very early assignment, we need a mode change.
		static const L3ChannelMode mode(L3ChannelMode::SpeechV1);
		LCH->send(L3ChannelModeModify(LCH->channelDescription(),mode));
		L3Message* msg_ack = getMessage(LCH);
		const L3ChannelModeModifyAcknowledge *ack =
			dynamic_cast<L3ChannelModeModifyAcknowledge*>(msg_ack);
		if (!ack) {
			if (msg_ack) {
				LOG(WARN) << "Unexpected message " << *msg_ack;
				delete msg_ack;
			}
			throw UnexpectedMessage(transaction.ID());
		}
		// Cause 0x06 is "channel unacceptable"
		bool modeOK = (ack->mode()==mode);
		delete msg_ack;
		if (!modeOK) return abortCall(transaction,LCH,L3Cause(0x06));
		MTCController(transaction,dynamic_cast<TCHFACCHLogicalChannel*>(LCH));
	}
	else {
		// For late assignment, send the TCH assignment now.
		// This dispatcher on the next channel will continue the transaction.
		assignTCHF(transaction,dynamic_cast<SDCCHLogicalChannel*>(LCH),TCH);
	}
}


void Control::MTCController(TransactionEntry& transaction, TCHFACCHLogicalChannel* TCH)
{
	// Early Assignment Mobile Terminated Call. 
	// Transaction table in 04.08 7.3.3 figure 7.10a

	LOG(DEBUG) << "transaction: " << transaction;
	unsigned L3TI = transaction.TIValue();
	assert(transaction.TIFlag()==1);
	assert(TCH);

	// Get the alerting message.
	LOG(INFO) << "waiting for GSM Alerting and Connect";
	while (transaction.Q931State()!=TransactionEntry::Active) {
		if (updateGSMSignalling(transaction,TCH,1000)) return;
		if (transaction.Q931State()==TransactionEntry::Active) break;
		if (transaction.Q931State()==TransactionEntry::CallReceived) {
			LOG(DEBUG) << "sending SIP Ringing";
			transaction.SIP().MTCSendRinging();
		}
		// Check for SIP cancel, too.
		if (transaction.SIP().MTCWaitForACK()==SIP::Fail) {
			return abortCall(transaction,TCH,L3Cause(0x7F));
		}
	}
	gTransactionTable.update(transaction);

	LOG(INFO) << "allocating port and sending SIP OKAY";
	unsigned RTPPorts = allocateRTPPorts();
	SIPState state = transaction.SIP().MTCSendOK(RTPPorts,SIP::RTPGSM610);
	while (state!=SIP::Active) {
		LOG(DEBUG) << "wait for SIP OKAY-ACK";
		if (updateGSMSignalling(transaction,TCH)) return;
		state = transaction.SIP().MTCWaitForACK();
		LOG(DEBUG) << "SIP call state "<< state;
		switch (state) {
			case SIP::Active:
				break;
			case SIP::Fail:
				return abortCall(transaction,TCH,L3Cause(0x7F));
			case SIP::Timeout:
				state = transaction.SIP().MTCSendOK(RTPPorts,SIP::RTPGSM610);
				break;
			case SIP::Connecting:
				break;
			default:
				LOG(NOTICE) << "SIP unexpected state " << state;
				break;
		}
	}
	transaction.SIP().MTCInitRTP();
	gTransactionTable.update(transaction);

	// Send Connect Ack to make it all official.
	LOG(DEBUG) << "MTC send GSM Connect Acknowledge";
	TCH->send(L3ConnectAcknowledge(0,L3TI));

	// At this point, everything is ready to run for the call.
	// The radio link should have been cleared with the call.
	gTransactionTable.update(transaction);
	callManagementLoop(transaction,TCH);
}




void Control::EmergencyCall(const L3CMServiceRequest* req, LogicalChannel *LCH)
{
	assert(req);
	LOG(ALARM) << "starting emergency call from request " << *req;
	assert(LCH);
	TCHFACCHLogicalChannel* TCH = dynamic_cast<TCHFACCHLogicalChannel*>(LCH);
	assert(TCH);

	// If we got a TMSI, find the IMSI.
	L3MobileIdentity mobileIdentity = req->mobileIdentity();
	if (mobileIdentity.type()==TMSIType) {
		const char *IMSI = gTMSITable.IMSI(mobileIdentity.TMSI());
		if (IMSI) mobileIdentity = L3MobileIdentity(IMSI);
	}

	// Can't find the TMSI?  Ask for an IMSI.
	if (mobileIdentity.type()==TMSIType) {
		LOG(NOTICE) << "E-MOC with no IMSI or IMEI.  Reqesting IMSI.";
		TCH->send(L3IdentityRequest(IMSIType));
		// FIXME -- This request times out on T3260, 12 sec.  See GSM 04.08 Table 11.2.
		L3Message* msg_resp = getMessage(TCH);
		L3IdentityResponse *resp = dynamic_cast<L3IdentityResponse*>(msg_resp);
		if (!resp) {
			if (msg_resp) {
				LOG(WARN) << "Unexpected message " << *msg_resp;
				delete msg_resp;
			}
			throw UnexpectedMessage();
		}
		mobileIdentity = resp->mobileID();
		delete msg_resp;
	}

	// Still no valid ID??  Get the IMEI.
	if (mobileIdentity.type()==TMSIType) {
		LOG(NOTICE) << "E-MOC with no IMSI or IMEI.  Reqesting IMEI.";
		TCH->send(L3IdentityRequest(IMSIType));
		// FIXME -- This request times out on T3260, 12 sec.  See GSM 04.08 Table 11.2.
		L3Message* msg_resp = getMessage(TCH);
		L3IdentityResponse *resp = dynamic_cast<L3IdentityResponse*>(msg_resp);
		if (!resp) {
			if (msg_resp) {
				LOG(WARN) << "Unexpected message " << *msg_resp;
				delete msg_resp;
			}
			throw UnexpectedMessage();
		}
		mobileIdentity = resp->mobileID();
		delete msg_resp;
	}

	// Still no valid ID???  F*, just make something up!
	if (mobileIdentity.type()==TMSIType) {
		LOG(WARN) << "E-MOC with no identity, forcing to null IMSI.";
		mobileIdentity = L3MobileIdentity("000000000000000");
	}

	// Let the phone know we're going ahead with the transaction.
	LOG(NOTICE) << "sending CMServiceAccept";
	TCH->send(L3CMServiceAccept());

	// Get the Setup message.
	L3Message* msg_setup = getMessage(TCH);
	const L3EmergencySetup *setup = dynamic_cast<const L3EmergencySetup*>(msg_setup);
	if (!setup) {
		if (msg_setup) {
			LOG(WARN) << "Unexpected message " << *msg_setup;
			delete msg_setup;
		}
		throw UnexpectedMessage();
	}
	LOG(NOTICE) << *setup;
	// Pull out the L3 short transaction information now.
	// See GSM 04.07 11.2.3.1.3.
	unsigned L3TI = setup->TIValue();

	// Make a copy.  Don't forget to delete it later.
	char *bcd_digits = strdup(gConfig.getStr("PBX.Emergency"));

	LOG(DEBUG) << "SIP start engine";
	// Create a transaction table entry so the TCH controller knows what to do later.
	// The transaction on the TCH is a continuation of this one and uses the same ID
	TransactionEntry transaction(mobileIdentity,
		req->serviceType(),
		L3TI, L3CalledPartyBCDNumber(bcd_digits));
	assert(transaction.TIFlag()==0);
	if (mobileIdentity.type()!=TMSIType) transaction.SIP().User(mobileIdentity.digits());
	transaction.Q931State(TransactionEntry::MOCInitiated);
	TCH->transactionID(transaction.ID());
	LOG(DEBUG) << "transaction: " << transaction;
	gTransactionTable.add(transaction);

	// Done with the setup message.
	delete msg_setup;

	// Now start a call by contacting asterisk.
	// Engine methods will return their current state.	
	// The remote party will start ringing soon.
	LOG(NOTICE) << "starting SIP (INVITE) Calling "<<bcd_digits;
	unsigned basePort = allocateRTPPorts();
	SIPState state = transaction.SIP().MOCSendINVITE(bcd_digits,gConfig.getStr("SIP.IP"),basePort,SIP::RTPGSM610);
	LOG(DEBUG) << "SIP state="<<state;
	LOG(DEBUG) << "Q.931 state=" << transaction.Q931State();

	free(bcd_digits);

	// For very early assignment, we need a mode change.
	static const L3ChannelMode mode(L3ChannelMode::SpeechV1);
	TCH->send(L3ChannelModeModify(TCH->channelDescription(),mode));
	L3Message *msg_ack = getMessage(TCH);
	const L3ChannelModeModifyAcknowledge *ack =
		dynamic_cast<L3ChannelModeModifyAcknowledge*>(msg_ack);
	if (!ack) {
		if (msg_ack) {
			LOG(WARN) << "Unexpected message " << *msg_ack;
			delete msg_ack;
		}
		throw UnexpectedMessage(transaction.ID());
	}
	// Cause 0x06 is "channel unacceptable"
	bool modeOK = (ack->mode()==mode);
	delete msg_ack;
	if (!modeOK) return abortCall(transaction,TCH,L3Cause(0x06));

	// From here on, it's normal call setup.
	MOCController(transaction,TCH);
}




void Control::TestCall(TransactionEntry& transaction, LogicalChannel *LCH)
{
	assert(LCH);
	LOG(INFO) << LCH->type() << " transaction: "<< transaction;

	// Mark the call as active.
	transaction.Q931State(TransactionEntry::Active);
	gTransactionTable.update(transaction);

	// Create and open the control port.
	UDPSocket controlSocket(gConfig.getNum("TestCall.Port"));

	// If this is a FACCH, change the mode from signaling-only to speech.
	if (LCH->type()==FACCHType) {
		static const L3ChannelMode mode(L3ChannelMode::SpeechV1);
		LCH->send(L3ChannelModeModify(LCH->channelDescription(),mode));
		L3Message *msg_ack = getMessage(LCH);
		const L3ChannelModeModifyAcknowledge *ack =
			dynamic_cast<L3ChannelModeModifyAcknowledge*>(msg_ack);
		if (!ack) {
			if (msg_ack) {
				LOG(WARN) << "Unexpected message " << *msg_ack;
				delete msg_ack;
			}
			controlSocket.close();
			throw UnexpectedMessage(transaction.ID());
		}
		// Cause 0x06 is "channel unacceptable"
		bool modeOK = (ack->mode()==mode);
		delete msg_ack;
		if (!modeOK) {
			controlSocket.close();
			return abortCall(transaction,LCH,L3Cause(0x06));
		}
	}

	assert(transaction.TIFlag()==1);

	// FIXME -- Somehow, the RTP ports need to be attached to the transaction.

	// This loop will run or block until some outside entity writes a
	// channel release on the socket.

	LOG(INFO) << "entering test loop";
	while (true) {
		// Get the outgoing message from the test call port.
		char iBuf[MAX_UDP_LENGTH];
		int msgLen = controlSocket.read(iBuf);
		LOG(INFO) << "got " << msgLen << " bytes on UDP";
		// Send it to the handset.
		L3Frame query(iBuf,msgLen);
		LOG(INFO) << "sending " << query;
		LCH->send(query);
		// Wait for a response.
		// FIXME -- This should be a proper T3xxx value of some kind.
		L3Frame* resp = LCH->recv(30000);
		if (!resp) {
			LOG(NOTICE) << "read timeout";
			break;
		}
		if (resp->primitive() != DATA) {
			LOG(NOTICE) << "unexpected primitive " << resp->primitive();
			break;
		}
		LOG(INFO) << "received " << *resp;
		// Send response on the port.
		unsigned char oBuf[resp->size()];
		resp->pack(oBuf);
		controlSocket.writeBack((char*)oBuf);
		// Delete and close the loop.
		delete resp;
	}
	controlSocket.close();
	LOG(INFO) << "ending";
	LCH->send(L3ChannelRelease());
	LCH->send(RELEASE);
	clearTransactionHistory(transaction);
}



void Control::initiateMTTransaction(TransactionEntry& transaction, GSM::ChannelType chanType, unsigned pageTime)
{
	gTransactionTable.add(transaction);
	gBTS.pager().addID(transaction.subscriber(),chanType,transaction,pageTime);
}





// vim: ts=4 sw=4
