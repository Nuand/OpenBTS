/**@file SMS Control (L3), GSM 03.40, 04.11. */
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


/*
	Abbreviations:
	MOSMS -- Mobile Originated Short Message Service
	MTSMS -- Mobile Terminated Short Message Service

	Verbs:
	"send" -- Transfer to the network.
	"receive" -- Transfer from the network.
	"submit" -- Transfer from the MS.
	"deliver" -- Transfer to the MS.

	MOSMS: The MS "submits" a message that OpenBTS then "sends" to the network.
	MTSMS: OpenBTS "receives" a message from the network that it then "delivers" to the MS.

*/


#include <stdio.h>
#include <GSMLogicalChannel.h>
#include <GSML3MMMessages.h>
#include "ControlCommon.h"
#include <Regexp.h>


using namespace std;
using namespace GSM;
using namespace Control;

#include "SMSMessages.h"
using namespace SMS;

#include "SIPInterface.h"
#include "SIPUtility.h"
#include "SIPMessage.h"
#include "SIPEngine.h"
using namespace SIP;

#include "CollectMSInfo.h"

/**
	Read an L3Frame from SAP3.
	Throw exception on failure.  Will NOT return a NULL pointer.
*/
L3Frame* getFrameSMS(LogicalChannel *LCH, GSM::Primitive primitive=DATA)
{
	L3Frame *retVal = LCH->recv(LCH->N200()*T200ms,3);
	if (!retVal) {
		throw ChannelReadTimeout();
	}
	LOG(DEBUG) << "getFrameSMS: " << *retVal;
	if (retVal->primitive() != primitive) {
		LOG(NOTICE) << "unexpected primitive, expecting " << primitive << ", got " << *retVal;
		throw UnexpectedPrimitive();
	}
	if ((retVal->primitive() == DATA) && (retVal->PD() != L3SMSPD)) {
		LOG(NOTICE) << "unexpected (non-SMS) protocol in frame " << retVal;
		throw UnexpectedMessage();
	}
	return retVal;
}





/**@name Functions for transmitting through various gateways. */
//@{


/** Substitute spaces with +'s for compatibility with some protocols. */
void convertText(char *dest, const char *src)
{
	// FIXME -- We should just do full "percent encoding" here.
	while (*src != '\0') {
		if (*src == ' ') *dest++ = '+';
		else *dest++ = *src;
		src++;
	}
	*dest = '\0';
}



/** Send SMS via and HTTP interface. */
bool sendHTTP(const char* destination, const char* message)
{
	char convMessage[strlen(message)+2];
	convertText(convMessage,message);
	char command[2048];
	// FIXME -- Check specs for a good timeout value here.
	sprintf(command,"wget -T 5 -C -q -O - \"http://%s/http/%s&to=%s&text=%s\" >& /dev/null",
		gConfig.getStr("SMS.HTTP.Gateway"),
		gConfig.getStr("SMS.HTTP.AccessString"),
		destination, convMessage);
	LOG(DEBUG) << "MOSMS: sendHTTP sending with " << command;

	// HTTP "GET" method with wget.
	// FIXME -- Look at the output of wget to check success.
	FILE* wget = popen(command,"r");
	if (!wget) {
		LOG(ALARM) << "cannot open wget with " << command;
		return false;
	}
	pclose(wget);
	return true;
}


/** Send e-mail with local sendmail program. */
bool sendEMail(const char* address, const char* body, const char* subject=NULL)
{
	// We're not checking for overflow because the TPDU can't be more than a few hundred bytes.

	// Build the command line.
	// FIXME -- Use sendmail to have better header control.
	char command[1024];
	if (subject) sprintf(command,"mail -s \"%s\" %s",subject,address);
	else sprintf(command,"mail %s",address);
	LOG(INFO) << "sending SMTP: \"" << body << "\" via \"" << command << "\"";

	// Send the mail.
	FILE* mail = popen(command,"w");
	if (!mail) {
		LOG(ALARM) << "cannot send mail with \"" << command << "\"";
		return false;
	}
	// FIXME -- We should be sure body is 7-bit clean.
	fprintf(mail,"%s",body);
	if (pclose(mail) == -1) return false;
	return true;
}


//@}




/**
	Send a TDPU to a numeric address.
	@param mobileID The sender's IMSI.
	@param submit A TL-SUBMIT PDU.
	@return true on success
*/
bool sendToNumericAddress(const L3MobileIdentity &mobileID, const TLSubmit& submit)
{
	LOG(INFO) << "from " << mobileID << " message: " << submit;
	const TLAddress& address = submit.DA();
	const char* body = submit.UD().data();

	// If there is an external HTTP gateway, use it.
	if (gConfig.defines("SMS.HTTP.Gateway")) return sendHTTP(address.digits(), body);

	// Otherwise, we are looking for a SIP interface to smsd.

	// Steps:
	// 1 -- Create a transaction record.
	// 2 -- Send it to the server.
	// 3 -- Wait for response or timeout.
	// 4 -- Return true for OK or ACCEPTED, false otherwise.

	// Form the TLAddress into a CalledPartyNumber for the transaction.
	L3CalledPartyBCDNumber calledParty(address.digits());
	// Step 1 -- Create a transaction record.
	TransactionEntry transaction(
		mobileID,
		L3CMServiceType::ShortMessage,
		0,		// doesn't matter
		calledParty);
	transaction.SIP().User(mobileID.digits());
	transaction.Q931State(TransactionEntry::SMSSubmitting);
	gTransactionTable.add(transaction);
	LOG(DEBUG) << "MOSMS: transaction: " << transaction;

	// Step 2 -- Send the message to the server.
	transaction.SIP().MOSMSSendMESSAGE(address.digits(),gConfig.getStr("SIP.IP"),body);

	// Step 3 -- Wait for OK or ACCEPTED.
	SIPState state = transaction.SIP().MOSMSWaitForSubmit();

	// Step 4 -- Done
	clearTransactionHistory(transaction);
	return state==SIP::Cleared;
}




/** Send a TPDU through whatever gateway is available.  */
bool submitSMS(const L3MobileIdentity& mobileID, const TLSubmit& submit)
{
	LOG(INFO) << "from " << mobileID;
	//const TLAddress& address = submit.DA();
	const char* body = submit.UD().data();

	// Check for direct e-mail address at start of message body.
	// FIXME -- This doesn't really follow the spec.  See GSM 03.40 3.8.
	static const Regexp emailAddress("^[[:graph:]]+@[[:graph:]]+ ");
	if (emailAddress.match(body)) {
		// FIXME -- Get the sender's E.164 to put in the subject line.
		char bodyCopy[strlen(body)+2];
		strcpy(bodyCopy,body);
		char* SMTPAddress = bodyCopy;
		char* term = strchr(bodyCopy,' ');
		// If term's NULL, the regexp is broken.
		assert(term);
		*term = '\0';
		char* SMTPPayload = term+1;
		LOG(INFO) << "sending SMTP to " << SMTPAddress << ": " << SMTPPayload;
		if (SMTPPayload) return sendEMail(SMTPAddress,SMTPPayload,"from OpenBTS gateway");
		else return sendEMail(SMTPAddress,"(empty)","from OpenBTS gateway");
	}

	// Send to smsd or HTTP gateway, depending on what's defined in the conig.
	return sendToNumericAddress(mobileID,submit);
}



/**
	Process the incoming TPDU.
	@param mobileID Sender's IMSI.
	@param TPDU The TPDU (duh).
	@return true if successful
*/
bool handleTPDU(const L3MobileIdentity& mobileID, const TLFrame& TPDU)
{
	LOG(DEBUG) << "SMS: handleTPDU MTI=" << TPDU.MTI();
	// Handle just the uplink cases.
	switch ((TLMessage::MessageType)TPDU.MTI()) {
		case TLMessage::DELIVER_REPORT:
		case TLMessage::STATUS_REPORT:
			return false;
		case TLMessage::SUBMIT: {
			TLSubmit submit;
			submit.parse(TPDU);
			LOG(INFO) << "SMS SMS-SUBMIT " << submit;
			return submitSMS(mobileID,submit);
		}
		default:
			return false;
	}
}



/**
	Process the RPDU.
	@param mobileID The sender's IMSI.
	@param RPDU The RPDU to process.
	@return true if successful.
*/
bool handleRPDU(const L3MobileIdentity& mobileID, const RLFrame& RPDU)
{
	LOG(DEBUG) << "SMS: handleRPDU MTI=" << RPDU.MTI();
	switch ((RPMessage::MessageType)RPDU.MTI()) {
		case RPMessage::Data: {
			RPData data;
			data.parse(RPDU);
			LOG(INFO) << "SMS RP-DATA " << data;
			return handleTPDU(mobileID,data.TPDU());
		}
		case RPMessage::Ack:
		case RPMessage::SMMA:
			return true;
		case RPMessage::Error:
		default:
			return false;
	}
}




void Control::MOSMSController(const L3CMServiceRequest *req, 
						LogicalChannel *LCH)
{
	assert(req);
	assert(LCH);

	LOG(INFO) << "MOSMS, req " << *req;

	// If we got a TMSI, find the IMSI.
	// Note that this is a copy, not a reference.
	L3MobileIdentity mobileIdentity = req->mobileIdentity();
	resolveIMSI(mobileIdentity,LCH);


	// See GSM 04.11 Arrow Diagram A5 for the transaction
	// Step 1	MS->Network	CP-DATA containing RP-DATA
	// Step 2	Network->MS	CP-ACK
	// Step 3	Network->MS	CP-DATA containing RP-ACK
	// Step 4	MS->Network	CP-ACK

	// LAPDm operation, from GSM 04.11, Annex F:
	// """
	// Case A: Mobile originating short message transfer, no parallel call:
	// The mobile station side will initiate SAPI 3 establishment by a SABM command
	// on the SDCCH after the cipher mode has been set. If no hand over occurs, the
	// SAPI 3 link will stay up until the last CP-ACK is received by the MSC, and
	// the clearing procedure is invoked.
	// """

	// FIXME: check provisioning

	// Let the phone know we're going ahead with the transaction.
	LOG(INFO) << "sending CMServiceAccept";
	LCH->send(L3CMServiceAccept());
	// Wait for SAP3 to connect.
	// The first read on SAP3 is the ESTABLISH primitive.
	delete getFrameSMS(LCH,ESTABLISH);

	// Step 1
	// Now get the first message.
	// Should be CP-DATA, containing RP-DATA.
	L3Frame *CM = getFrameSMS(LCH);
	LOG(DEBUG) << "data from MS " << *CM;
	if (CM->MTI()!=CPMessage::DATA) {
		LOG(NOTICE) << "unexpected SMS CP message with TI=" << CM->MTI();
		throw UnexpectedMessage();
	}
	unsigned TI = CM->TIValue();

	// Step 2
	// Respond with CP-ACK.
	// This just means that we got the message.
	LOG(INFO) << "sending CPAck";
	LCH->send(CPAck(1,TI),3);

	// Parse the message in CM and process RP part.
	// This is where we actually parse the message and send it out.
	// FIXME -- We need to set the message ref correctly,
	// even if the parsing fails.
	// The compiler gives a warning here.  Let it.  It will remind someone to fix it.
	unsigned ref;
	bool success = false;
	try {
		CPData data;
		data.parse(*CM);
		delete CM;
		LOG(INFO) << "CPData " << data;
		// Transfer out the RPDU -> TPDU -> delivery.
		ref = data.RPDU().reference();
		// This handler invokes higher-layer parsers, too.
		success = handleRPDU(mobileIdentity,data.RPDU());
	}
	catch (SMSReadError) {
		LOG(WARN) << "SMS parsing failed (above L3)";
		// Cause 95, "semantically incorrect message".
		LCH->send(CPData(1,TI,RPError(95,ref)),3);
		throw UnexpectedMessage();
	}
	catch (L3ReadError) {
		LOG(WARN) << "SMS parsing failed (in L3)";
		throw UnsupportedMessage();
	}

	// Step 3
	// Send CP-DATA containing RP-ACK and message reference.
	if (success) {
		LOG(INFO) << "sending RPAck in CPData";
		LCH->send(CPData(1,TI,RPAck(ref)),3);
	} else {
		LOG(INFO) << "sending RPError in CPData";
		// Cause 127 is "internetworking error, unspecified".
		// See GSM 04.11 Table 8.4.
		LCH->send(CPData(1,TI,RPError(127,ref)),3);
	}

	// Step 4
	// Get CP-ACK from the MS.
	CM = getFrameSMS(LCH);
	if (CM->MTI()!=CPMessage::ACK) {
		LOG(NOTICE) << "unexpected SMS CP message with TI=" << CM->MTI();
		throw UnexpectedMessage();
	}
	LOG(DEBUG) << "ack from MS: " << *CM;
	CPAck ack;
	ack.parse(*CM);
	LOG(INFO) << "CPAck " << ack;

	// RRLP Here if enabled
	if (gConfig.defines("GSM.RRLP") && gConfig.getNum("GSM.RRLP") == 1 &&
		gConfig.defines("RRLP.LocationUpdate") && gConfig.getNum("RRLP.LocationUpdate") == 1 /* RRLP? */)
	{
		Timeval start;
		RRLP::collectMSInfo(mobileIdentity, LCH, true /* DO RRLP */);
		LOG(INFO) << "submitSMS with RRLP took " << start.elapsed() << " for IMSI " << mobileIdentity;
	}

	// Done.
	LOG(INFO) << "closing";
	LCH->send(L3ChannelRelease());
}




bool Control::deliverSMSToMS(const char *callingPartyDigits, const char* message, unsigned TI, LogicalChannel *LCH)
{
	// This function is used to deliver messages that originate INSIDE the BTS.
	// For the normal SMS delivery, see MTSMSController.

	// Start ABM in SAP3.
	LCH->send(ESTABLISH,3);
	// Wait for SAP3 ABM to connect.
	// The next read on SAP3 should the ESTABLISH primitive.
	// This won't return NULL.  It will throw an exception if it fails.
	delete getFrameSMS(LCH,ESTABLISH);

	// HACK
	// Check for "Easter Eggs"
	// TL-PID
	// See 03.40 9.2.3.9.
	unsigned TLPID=0;
	if (strncmp(message,"#!TLPID",7)==0) sscanf(message,"#!TLPID%d",&TLPID);

	// Step 1
	// Send the first message.
	// CP-DATA, containing RP-DATA.
	unsigned reference = random() % 255;
	CPData deliver(0,TI,
		RPData(reference,
			RPAddress(gConfig.getStr("SMS.FakeSrcSMSC")),
			TLDeliver(callingPartyDigits,message,TLPID)));
	LOG(INFO) << "sending " << deliver;
	LCH->send(deliver,3);

	// Step 2
	// Get the CP-ACK.
	// FIXME -- Check TI.
	LOG(DEBUG) << "MTSMS: waiting for CP-ACK";
	L3Frame *CM = getFrameSMS(LCH);
	LOG(DEBUG) << "MTSMS: ack from MS " << *CM;
	if (CM->MTI()!=CPMessage::ACK) {
		LOG(WARN) << "MS rejected our RP-DATA with CP message with TI=" << CM->MTI();
		throw UnexpectedMessage();
	}

	// Step 3
	// Get CP-DATA containing RP-ACK and message reference.
	LOG(DEBUG) << "MTSMS: waiting for RP-ACK";
	CM = getFrameSMS(LCH);
	LOG(DEBUG) << "MTSMS: data from MS " << *CM;
	if (CM->MTI()!=CPMessage::DATA) {
		LOG(NOTICE) << "Unexpected SMS CP message with TI=" << CM->MTI();
		throw UnexpectedMessage();
	}

	// FIXME -- Check TI.

	// Parse to check for RP-ACK.
	CPData data;
	try {
		data.parse(*CM);
		delete CM;
		LOG(DEBUG) << "CPData " << data;
	}
	catch (SMSReadError) {
		LOG(WARN) << "SMS parsing failed (above L3)";
		// Cause 95, "semantically incorrect message".
		LCH->send(CPError(0,TI,95),3);
		throw UnexpectedMessage();
	}
	catch (L3ReadError) {
		LOG(WARN) << "SMS parsing failed (in L3)";
		throw UnsupportedMessage();
	}

	// FIXME -- Check SMS reference.

	bool success = true;
	if (data.RPDU().MTI()!=RPMessage::Ack) {
		LOG(WARN) << "unexpected RPDU " << data.RPDU();
		success = false;
	}

	// Step 4
	// Send CP-ACK to the MS.
	LOG(INFO) << "MTSMS: sending CPAck";
	LCH->send(CPAck(0,TI),3);
	return success;
}



// Some utils for the RRLP hack below
int hexchr2int(char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'z')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'Z')
		return c - 'A' + 10;
	return -10000;
}

BitVector hex2bitvector(const char* s)
{
	BitVector ret(strlen(s)*4);
	size_t write_pos = 0;
	for (;*s != 0 && *(s+1) != 0; s+= 2) {
		ret.writeField(write_pos, hexchr2int(*s), 4);
		ret.writeField(write_pos, hexchr2int(*(s+1)), 4);
	}
	return ret;
}


void Control::MTSMSController(TransactionEntry& transaction, 
						LogicalChannel *LCH)
{
	assert(LCH);

	// HACK: At this point if the message starts with "RRLP" then we don't do SMS at all,
	// but instead to an RRLP transaction over the already allocated LogicalChannel.
	const char* m = transaction.message(); // NOTE - not very nice, my way of checking.
	if ((strlen(m) > 4) && (std::string("RRLP") == std::string(m, m+4))) {
		BitVector rrlp_position_request = hex2bitvector(transaction.message() + 4);
		LOG(INFO) << "MTSMS: Sending RRLP";
		// TODO - how to get mobID here?
		L3MobileIdentity mobID = L3MobileIdentity("000000000000000");
		RRLP::PositionResult pr = GSM::RRLP::doRRLPQuery(mobID, LCH, rrlp_position_request);
		if (pr.mValid) // in this case we only want to log the results which contain lat/lon
			logMSInfo(LCH, pr, mobID);
		LOG(INFO) << "MTSMS: Closing channel after RRLP";
		LCH->send(L3ChannelRelease());
		clearTransactionHistory(transaction);
		return;
	}

	// See GSM 04.11 Arrow Diagram A5 for the transaction
	// Step 1	Network->MS	CP-DATA containing RP-DATA
	// Step 2	MS->Network	CP-ACK
	// Step 3	MS->Network	CP-DATA containing RP-ACK
	// Step 4	Network->MS	CP-ACK

	// LAPDm operation, from GSM 04.11, Annex F:
	// """
	// Case B: Mobile terminating short message transfer, no parallel call:
	// The network side, i.e. the BSS will initiate SAPI3 establishment by a
	// SABM command on the SDCCH when the first CP-Data message is received
	// from the MSC. If no hand over occurs, the link will stay up until the
	// MSC has given the last CP-ack and invokes the clearing procedure. 
	// """

	LOG(INFO) << "MTSMS: transaction: "<< transaction;
	LCH->transactionID(transaction.ID());	
	SIPEngine& engine = transaction.SIP();

	// Update transaction state.
	transaction.Q931State(TransactionEntry::SMSDelivering);
	gTransactionTable.update(transaction);

	bool success = deliverSMSToMS(transaction.calling().digits(),transaction.message(),random()%7,LCH);

	// Close the Dm channel.
	LOG(INFO) << "MTSMS: closing";
	LCH->send(L3ChannelRelease());

	// Ack in SIP domain and update transaction state.
	if (success) {
		engine.MTSMSSendOK();
		clearTransactionHistory(transaction);
	}
}





// vim: ts=4 sw=4
