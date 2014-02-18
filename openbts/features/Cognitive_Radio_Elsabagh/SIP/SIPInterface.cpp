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




#include <ortp/ortp.h>
#include <osipparser2/sdp_message.h>

#include "GSMConfig.h"
#include "ControlCommon.h"

#include "Sockets.h"

#include "SIPUtility.h"
#include "SIPInterface.h"

#include <Logger.h>



using namespace std;
using namespace SIP;

using namespace GSM;
using namespace Control;



static const char emptyString[] = "";


// SIPMessageMap method definitions.

void SIPMessageMap::write(const std::string& call_id, osip_message_t * msg)
{
	LOG(DEBUG) << "call_id=" << call_id << " msg=" << msg;
	OSIPMessageFIFO * fifo = mMap.readNoBlock(call_id);
	if( fifo==NULL ) {
		// FIXME -- If this write fails, send "call leg non-existent" response on SIP interface.
		LOG(NOTICE) << "missing SIP FIFO "<<call_id;
		throw SIPError();
	}
	LOG(DEBUG) << "write on fifo " << fifo;
	fifo->write(msg);	
}

osip_message_t * SIPMessageMap::read(const std::string& call_id, unsigned readTimeout)
{ 
	LOG(DEBUG) << "call_id=" << call_id;
	OSIPMessageFIFO * fifo = mMap.readNoBlock(call_id);
	if (!fifo) {
		LOG(NOTICE) << "missing SIP FIFO "<<call_id;
		throw SIPError();
	}	
	LOG(DEBUG) << "blocking on fifo " << fifo;
	osip_message_t * msg =  fifo->read(readTimeout);	
	if (!msg) throw SIPTimeout();
	return msg;
}


bool SIPMessageMap::add(const std::string& call_id, const struct sockaddr_in* returnAddress)
{
	OSIPMessageFIFO * fifo = new OSIPMessageFIFO(returnAddress);
	mMap.write(call_id, fifo);
	return true;
}

bool SIPMessageMap::remove(const std::string& call_id)
{
	OSIPMessageFIFO * fifo = mMap.readNoBlock(call_id);
	if(fifo == NULL) return false;
	mMap.remove(call_id);
	return true;
}





// SIPInterface method definitions.

bool SIPInterface::addCall(const string &call_id)
{
	LOG(INFO) << "creating SIP message FIFO callID " << call_id;
	return mSIPMap.add(call_id,mSIPSocket.source());
}


bool SIPInterface::removeCall(const string &call_id)
{
	LOG(INFO) << "removing SIP message FIFO callID " << call_id;
	return mSIPMap.remove(call_id);
}

int SIPInterface::fifoSize(const std::string& call_id )
{ 
	OSIPMessageFIFO * fifo = mSIPMap.map().read(call_id,0);
	if(fifo==NULL) return -1;
	return fifo->size();
}	



SIPInterface::SIPInterface()
	:mSIPSocket(gConfig.getNum("SIP.Port"), gConfig.getStr("Asterisk.IP"), gConfig.getNum("Asterisk.Port"))
{
	mAsteriskPort = gConfig.getNum("Asterisk.Port");
	mMessengerPort = gConfig.getNum("Smqueue.Port");
	assert(resolveAddress(&mAsteriskAddress,gConfig.getStr("Asterisk.IP"),mAsteriskPort));
	assert(resolveAddress(&mMessengerAddress,gConfig.getStr("Smqueue.IP"),mMessengerPort));
}


void SIP::driveLoop( SIPInterface * si){
	while (true) {
		si->drive();
	}
}

void SIPInterface::start(){
	// Start all the osip/ortp stuff. 
	parser_init();
	ortp_init();
	ortp_scheduler_init();
	// FIXME -- Can we coordinate this with the global logger?
	//ortp_set_log_level_mask(ORTP_MESSAGE|ORTP_WARNING|ORTP_ERROR);
	mDriveThread.start((void *(*)(void*))driveLoop,this );
}




void SIPInterface::write(const struct sockaddr_in* dest, osip_message_t *msg) 
{
	char * str;
	size_t msgSize;
	osip_message_to_str(msg, &str, &msgSize);
	if (!str) {
		LOG(ERROR) << "osip_message_to_str produced a NULL pointer.";
		return;
	}
	char line[1000];
	sscanf(str,"%[^\n]",line);
	LOG(INFO) << "write " << line;
	LOG(DEBUG) << "write " << str;

	mSocketLock.lock();
	mSIPSocket.send((const struct sockaddr*)dest,str);
	mSocketLock.unlock();
	free(str);
}



void SIPInterface::drive() 
{
	char buffer[2048];

	LOG(DEBUG) << "blocking on socket";
	int numRead = mSIPSocket.read(buffer);
	if (numRead<0) {
		LOG(ALARM) << "cannot read SIP socket.";
		return;
	}
	buffer[numRead+1] = '\0';

	char line[sizeof(buffer)];
	sscanf(buffer,"%[^\n]",line);
	LOG(INFO) << "read " << line;
	LOG(DEBUG) << "read " << buffer;

	try {

		// Parse the mesage.
		osip_message_t * msg;
		osip_message_init(&msg);
		osip_message_parse(msg, buffer, strlen(buffer));
	
		if (msg->sip_method) LOG(DEBUG) << "read method " << msg->sip_method;
	
		// Must check if msg is an invite.
		// if it is, handle appropriatly.
		// FIXME -- Check return value in case this failed.
		checkInvite(msg);

		// FIXME -- Need to check for early BYE to stop paging .
		// If it's a BYE, find the corresponding transaction table entry.
		// If the Q931 state is "paging", or T3113 is expired, remove it.
		// Otherwise, we keep paging even though the call has ended.
		
		// Multiplex out the received SIP message to active calls.

		// If we write to non-existent call_id.
		// this is errant message so need to catch
		// Internal error excatpion. and give nice
		// message (instead of aborting)
		// Don't free call_id_num.  It points into msg->call_id.
		char * call_id_num = osip_call_id_get_number(msg->call_id);	
		if( call_id_num == NULL ) {
			LOG(WARN) << "message with no call id";
			throw SIPError();
		}
		LOG(DEBUG) << "got message " << msg << " with call id " << call_id_num << " and writing it to the map.";
		string call_num(call_id_num);
		// FIXME -- If this write fails, send "call leg non-existent" response on SIP interface.
		mSIPMap.write(call_num, msg);
	}
	catch(SIPException) {
		sscanf(buffer,"%[^\n]",line);
		LOG(WARN) << "errant SIP Message: " << line;
	}
}




bool SIPInterface::checkInvite( osip_message_t * msg )
{
	LOG(DEBUG);

	// Is there even a method?
	const char *method = msg->sip_method;
	if (!method) return false;

	// Check for INVITE or MESSAGE methods.
	GSM::ChannelType requiredChannel;
	bool channelAvailable = false;
	GSM::L3CMServiceType serviceType;
	if (strcmp(method,"INVITE") == 0) {
		// INVITE is for MTC.
		// Set the required channel type to match the assignment style.
		if (gConfig.defines("GSM.VEA")) {
			// Very early assignment.
			requiredChannel = GSM::TCHFType;
			channelAvailable = gBTS.TCHAvailable();
		} else {
			// Early assignment
			requiredChannel = GSM::SDCCHType;
			channelAvailable = gBTS.SDCCHAvailable() && gBTS.TCHAvailable();
		}
		serviceType = L3CMServiceType::MobileTerminatedCall;
	}
	else if (strcmp(method,"MESSAGE") == 0) {
		// MESSAGE is for MTSMS.
		requiredChannel = GSM::SDCCHType;
		channelAvailable = gBTS.SDCCHAvailable();
		serviceType = L3CMServiceType::MobileTerminatedShortMessage;
	}
	else {
		// We must not handle this method.
		LOG(DEBUG) << "non-initiating SIP method " << method;
		return false;
	}

	// Check gBTS for channel availability.
	if (!channelAvailable) {
		// FIXME -- Send 503 "Service Unavailable" response on SIP interface.
		LOG(NOTICE) << "MTC CONGESTION, no " << requiredChannel << " availble for assignment";
		return false;
	}
	LOG(INFO) << "set up MTC paging for channel=" << requiredChannel;

	// Get call_id from invite message.
	if (!msg->call_id) {
		// FIXME -- Send appropriate error on SIP interface.
		LOG(WARN) << "Incoming INVITE/MESSAGE with no call ID";
		return false;
	}

	// Don't free call_id_num.  It points into msg->call_id.
	const char * call_id_num = osip_call_id_get_number(msg->call_id);	

	// Get request username (IMSI) from invite. 
	// Form of the name is IMSI<digits>, and it should always be 19 char.
	const char * IMSI = msg->req_uri->username;
	LOG(INFO) << msg->sip_method << " to "<< IMSI;
	// IMSIs are 14 or 15 char + "IMSI" prefix
	unsigned namelen = strlen(IMSI);
	if ((namelen>19)||(namelen<18)) {
		LOG(WARN) << "INVITE with malformed username \"" << IMSI << "\"";
		return false;
	}
	// Skip first 4 char "IMSI".
	IMSI+=4;
	// Make the mobile id we need for transaction and paging enties.
	L3MobileIdentity mobile_id(IMSI);

	// Check SIP map.  Repeated entry?  Page again.
	if (mSIPMap.map().readNoBlock(call_id_num) != NULL) { 
		TransactionEntry transaction;
		if (!gTransactionTable.find(mobile_id,transaction)) {
			// FIXME -- Send "call leg non-existent" response on SIP interface.
			LOG(WARN) << "repeated INVITE/MESSAGE with no transaction record";
			// Delete the bogus FIFO.
			mSIPMap.remove(call_id_num);
			return false;
		}
		LOG(INFO) << "repeated SIP INVITE/MESSAGE, repaging for transaction " << transaction; 
		gBTS.pager().addID(mobile_id,requiredChannel,transaction);	
		gTransactionTable.update(transaction);
		return false;
	}

	// Add an entry to the SIP Map to route inbound SIP messages.
	addCall(call_id_num);

	// Install transaction.
	LOG(INFO) << "make new transaction for " << mobile_id;
	// Put the caller ID in here if it's available.
	const char *callerID = NULL;
	const char *callerHost = NULL;
	osip_from_t *from = osip_message_get_from(msg);
	if (from) {
		osip_uri_t* url = osip_contact_get_url(from);
		if (url) {
			callerID = url->username;
			callerHost = url->host;
		}
	}
	if (!callerID) {
		callerID = emptyString;
		callerHost = emptyString;
		LOG(NOTICE) << "INVITE with no From: username for " << mobile_id;
	}
	LOG(DEBUG) << "callerID " << callerID << "@" << callerHost;
	// Build the transaction table entry.
	// This constructor sets TI flag=0, TI=0 for an MT transaction.
	TransactionEntry transaction(mobile_id,serviceType,callerID);
	LOG(DEBUG) << "call_id_num \"" << call_id_num << "\"";
	LOG(DEBUG) << "IMSI \"" << IMSI << "\"";

	transaction.SIP().User(call_id_num,IMSI,callerID,callerHost);
	transaction.SIP().saveINVITE(msg);
	if (serviceType == L3CMServiceType::MobileTerminatedShortMessage) {
		osip_body_t *body;
		osip_message_get_body(msg,0,&body);
		if (!body) return false;
		char *text = body->body;
		if (text) {
			transaction.message(text, body->length);
		}
		else LOG(NOTICE) << "MTSMS incoming MESSAGE method with no message body for " << mobile_id;
	}
	LOG(INFO) << "MTC MTSMS making transaction and add to transaction table: "<< transaction;
	gTransactionTable.add(transaction); 
	
	// Add to paging list and tell the remote SIP end that we are trying.
	LOG(DEBUG) << "MTC MTSMS new SIP invite, initial paging for mobile ID " << mobile_id;
	gBTS.pager().addID(mobile_id,requiredChannel,transaction);	
	// FIXME -- Send TRYING?  See MTCSendTrying for example.

	return true;
}





// vim: ts=4 sw=4
