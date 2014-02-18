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



#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include <sys/types.h>
#include <semaphore.h>

#include "GSMConfig.h"
#include "ControlCommon.h"
#include "GSMCommon.h"

#include "SIPInterface.h"
#include "SIPUtility.h"
#include "SIPMessage.h"
#include "SIPEngine.h"


using namespace std;
using namespace SIP;
using namespace GSM;
using namespace Control;





ostream& SIP::operator<<(ostream& os, SIP::SIPState s)
{
	switch(s)
	{	
		case NullState:		os<<"Null"; break;
		case Timeout :	 	os<<"Timeout"; break;
		case Starting : 	os<<"Starting"; break;
		case Proceeding : 	os<<"Proceeding"; break;
		case Ringing : 		os<<"Ringing"; break;
		case Connecting : 	os<<"Connecting"; break;
		case Active :		os<<"Active"; break;
		case Fail:			os<<"Fail"; break; 
		case Busy:			os<<"Busy"; break;
		case Clearing:		os<<"Clearing"; break;
		case Cleared:		os<<"Cleared"; break;
		case MessageSubmit: os<<"SMS-Submit"; break;
		default: os << "??" << (int)s << "??";
	}
	return os;
}



SIPEngine::~SIPEngine()
{
	if (mINVITE==NULL) osip_message_free(mINVITE);
	if (mOK==NULL) osip_message_free(mOK);
	if (mBYE==NULL) osip_message_free(mBYE);
}



void SIPEngine::saveINVITE(const osip_message_t *INVITE)
{
	// Duplicate the current invite.
	if (mINVITE!=NULL) osip_message_free(mINVITE);
	osip_message_clone(INVITE,&mINVITE);

	// First, get the from: field.
	osip_from_t *from = osip_message_get_from(INVITE);
	if (!from) {
		LOG(NOTICE) << "SIPEngine::  INVITE with no From: username";
		mFromTag = "";
		return;
	}

	// Get the from: tag.
	osip_uri_param_t * from_tag_param;
	osip_from_get_tag(from, &from_tag_param);
	LOG(DEBUG) << "SIPEngine:: from_tag_param =" <<from_tag_param;
	mFromTag = from_tag_param->gvalue;	
}



void SIPEngine::saveOK(const osip_message_t *OK)
{
	if (mOK!=NULL) osip_message_free(mOK);
	osip_message_clone(OK,&mOK);
}

void SIPEngine::saveBYE(const osip_message_t *BYE)
{
	if (mBYE!=NULL) osip_message_free(mBYE);
	osip_message_clone(BYE,&mBYE);
}



void SIPEngine::User( const char * IMSI )
{
	LOG(DEBUG) << "IMSI=" << IMSI;
	unsigned id = random();
	char tmp[20];
	sprintf(tmp, "%u", id);
	mCallID = tmp; 
	// IMSI gets prefixed with "IMSI" to form a SIP username
	mSIPUsername = string("IMSI") + IMSI;
}
	


void SIPEngine::User( const char * wCallID, const char * IMSI, const char *origID, const char *origHost) 
{
	LOG(DEBUG) << "IMSI=" << IMSI << " " << wCallID << " " << origID << "@" << origHost;  
	mSIPUsername = string("IMSI") + IMSI;
	mCallID = wCallID;
	mRemoteUsername = origID;
	mRemoteDomain = origHost;
}



bool SIPEngine::Register( Method wMethod )
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState << " " << wMethod << " callID " << mCallID;

	// Before start, need to add mCallID
	gSIPInterface.addCall(mCallID);

	// Initial configuration for sip message.
	// Make a new from tag and new branch.
	// make new mCSeq.
	char tmp[100];
	make_tag(tmp);
	mFromTag=tmp;	
	make_branch(tmp);
	mViaBranch=tmp;
	mCSeq = random()%600;
	
	// Generate SIP Message 
	// Either a register or unregister. Only difference 
	// is expiration period.
	osip_message_t * reg; 
	if (wMethod == SIPRegister ){
		reg = sip_register( mSIPUsername.c_str(), 
			gConfig.getNum("SIP.RegistrationPeriod"),
			mSIPPort, gConfig.getStr("SIP.IP"), 
			mAsteriskIP, mFromTag.c_str(), 
			mViaBranch.c_str(), mCallID.c_str(), mCSeq
		); 		
	} else if (wMethod == SIPUnregister ) {
		reg = sip_unregister( mSIPUsername.c_str(), 
			mSIPPort, gConfig.getStr("SIP.IP"), 
			mAsteriskIP, mFromTag.c_str(), 
			mViaBranch.c_str(), mCallID.c_str(), mCSeq
		);
	} else abort();
 
	// Write message and delete message to
	// prevent memory leak.	
	LOG(DEBUG) << "writing " << reg;
	gSIPInterface.writeAsterisk(reg);	
	osip_message_free(reg);

	// Poll the message FIFO until timeout or OK.
	// SIPInterface::read will throw SIPTIimeout if it times out.
	// It should not return NULL.
	try {
		static const int SIPTimeout = 10000;
		osip_message_t *msg = gSIPInterface.read(mCallID, SIPTimeout);
		assert(msg);
		while (msg->status_code!=200) {
			// Looking for 200 OK.
			// But will keep waiting if we get 1xx status mesasges, e.g. 100 Trying.
			LOG(DEBUG) << "received status " << msg->status_code << " " << msg->reason_phrase;
			if (msg->status_code>=300) {
				gSIPInterface.removeCall(mCallID);
				osip_message_free(msg);
				if (msg->status_code==404) {
					LOG(DEBUG) << "user not found";
				} else if (msg->status_code>=300 && msg->status_code<400) {
					LOG(WARN) << "REGISTER redirection requested but not implemented";
				} else {
					LOG(WARN) << "unexpected response " << msg->status_code << " " << msg->reason_phrase;
				}
				return false;
			}
			osip_message_free(msg);
			msg = gSIPInterface.read(mCallID, SIPTimeout);
			assert(msg);
		}
		LOG(DEBUG) << "success";
		gSIPInterface.removeCall(mCallID);
		osip_message_free(msg);
		return true;
	}
	catch (SIPTimeout) {
		LOG(ALARM) << "SIP register timed out.  Is Asterisk OK?";
		gSIPInterface.removeCall(mCallID);	
		throw SIPTimeout();
	}
}


SIPState SIPEngine::MOCSendINVITE( const char * wCalledUsername, 
	const char * wCalledDomain , short wRtp_port, unsigned  wCodec)
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	// Before start, need to add mCallID
	gSIPInterface.addCall(mCallID);
	
	// Set Invite params. 
	// New from tag + via branch
	// new CSEQ and codec 
	char tmp[100];
	make_tag(tmp);
	mFromTag = tmp;
	make_branch(tmp);
	mViaBranch = tmp;
	mCodec = wCodec;
	mCSeq++;

	mRemoteUsername = wCalledUsername;
	mRemoteDomain = wCalledDomain;
	mRTPPort= wRtp_port;
	
	LOG(DEBUG) << "mRemoteUsername=" << mRemoteUsername;
	LOG(DEBUG) << "mSIPUsername=" << mSIPUsername;

	osip_message_t * invite = sip_invite(
		mRemoteUsername.c_str(), mRTPPort, mSIPUsername.c_str(), 
		mSIPPort, gConfig.getStr("SIP.IP"), mAsteriskIP, 
		mFromTag.c_str(), mViaBranch.c_str(), mCallID.c_str(), mCSeq, mCodec); 
	
	// Send Invite to Asterisk.
	gSIPInterface.writeAsterisk(invite);
	saveINVITE(invite);
	osip_message_free(invite);
	mState = Starting;
	return mState;
};


SIPState SIPEngine::MOCResendINVITE()
{
	assert(mINVITE);
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	gSIPInterface.writeAsterisk(mINVITE);
	return mState;
}

SIPState  SIPEngine::MOCWaitForOK()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;

	osip_message_t * msg;

	// Read off the fifo. if time out will
	// clean up and return false.
	try {
		msg = gSIPInterface.read(mCallID, INVITETimeout);
	}
	catch (SIPTimeout& e) { 
		LOG(DEBUG) << "timeout";
		mState = Timeout;
		return mState;
	}

	LOG(DEBUG) << "received "<<msg->status_code;
	switch (msg->status_code) {
		case 100:
		case 183:
			LOG(DEBUG) << "TRYING/PROGRESS";
			mState = Proceeding;
			break;
		case 180:
			LOG(DEBUG) << "RINGING";
			mState = Ringing;
			
			osip_uri_param_t * mToTag_param;
			osip_to_get_tag(msg->to, &mToTag_param);
			mFromTag = mToTag_param->gvalue;
			
			osip_uri_param_t * mFromTag_param;        
			osip_from_get_tag(msg->from, &mFromTag_param);
			mToTag = mFromTag_param->gvalue;     
			LOG(DEBUG) << "RINGING: mToTag="<<mToTag;
			LOG(DEBUG) << "RINGING: mFromTag="<<mFromTag;

			break;
		case 200:
			LOG(DEBUG) << "OK";
			// save the OK message for the eventual ACK
			saveOK(msg);
			mState = Active;
			break;
		case 486:
			LOG(DEBUG) << "BUSY";
			mState = Busy;
			break;
		default:
			LOG(DEBUG) << "unhandled status code "<<msg->status_code;
			mState = Fail;
	}

	osip_message_free(msg);
	return mState;
}


SIPState SIPEngine::MOCSendACK()
{
	assert(mOK);
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	// make new via branch.
	char tmp[100];
	make_branch(tmp);
	mViaBranch = tmp;

	// get to tag from mOK message and copy to ack.
//	osip_uri_param_t * mToTag_param;
//	osip_to_get_tag(mOK->to, &mToTag_param);
//	mToTag = mToTag_param->gvalue;

	// get ip owner from mOK message.

	// HACK -- need to fix this crash
	get_owner_ip(mOK, tmp);
	mRemoteDomain = tmp;
	osip_message_t * ack;

	// Now ack the OK
	// HACK- setting owner address to localhost 
	// since we know its asterisk and get_owner_ip is
	// segfaulting. BUG []
	ack = sip_ack( mRemoteDomain.c_str(),
		mRemoteUsername.c_str(), 
		mSIPUsername.c_str(),
		mSIPPort, gConfig.getStr("SIP.IP"), mAsteriskIP, 
		mFromTag.c_str(), mToTag.c_str(), 
		mViaBranch.c_str(), mCallID.c_str(), mCSeq
	);

	gSIPInterface.writeAsterisk(ack);
	osip_message_free(ack);	
	LOG(DEBUG) << "call active";

	mState=Active;
	return mState;
}


SIPState SIPEngine::MODSendBYE()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	char tmp[50];
	make_branch(tmp);
	mViaBranch = tmp;
	mCSeq++;

	osip_message_t * bye = sip_bye(mRemoteDomain.c_str(), mRemoteUsername.c_str(), 
		mSIPUsername.c_str(),
		mSIPPort, gConfig.getStr("SIP.IP"), mAsteriskIP,
		mFromTag.c_str(), mToTag.c_str(), 
		mViaBranch.c_str(), mCallID.c_str(), mCSeq );

	gSIPInterface.writeAsterisk(bye);
	saveBYE(bye);
	osip_message_free(bye);
	mState = Clearing;
	return mState;
}

SIPState SIPEngine::MODResendBYE()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	assert(mState==Clearing);
	assert(mBYE);
	gSIPInterface.writeAsterisk(mBYE);
	return mState;
}

SIPState SIPEngine::MODWaitForOK()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	try {
		osip_message_t * ok = gSIPInterface.read(mCallID, BYETimeout);
		if(ok->status_code == 200 ) {
			mState = Cleared;
			// Remove Call ID at the end of interaction.
			gSIPInterface.removeCall(mCallID);	
		}
		osip_message_free(ok);
		return mState;	
	}

	catch (SIPTimeout& e) {
		LOG(NOTICE) << "timeout";
		return mState;
	}
}



SIPState SIPEngine::MTDCheckBYE()
{
	LOG(DEEPDEBUG) << "user " << mSIPUsername << " state " << mState;
	// If the call is not active, there should be nothing to check.
	if (mState!=Active) return mState;

	// Need to check size of osip_message_t* fifo,
	// so need to get fifo pointer and get size.
	// HACK -- reach deep inside to get damn thing
	int fifoSize = gSIPInterface.fifoSize(mCallID);


	// Size of -1 means the FIFO does not exist.
	// Treat the call as cleared.
	if (fifoSize==-1) {
		LOG(NOTICE) << "MTDCheckBYE attempt to check BYE on non-existant SIP FIFO";
		mState=Cleared;
		return mState;
	}

	// If no messages, there is no change in state.
	if (fifoSize==0) return mState;	
		
	osip_message_t * msg = gSIPInterface.read(mCallID);
	

	if ((msg->sip_method!=NULL) && (strcmp(msg->sip_method,"BYE")==0)) { 
		LOG(DEBUG) << "found msg="<<msg->sip_method;	
		saveBYE(msg);
		mState =  Clearing;
	}

	// FIXME -- Check for repeated ACK and send OK if needed.

	osip_message_free(msg);
	return mState;
} 


SIPState SIPEngine::MTDSendOK()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	assert(mBYE);
	osip_message_t * okay = sip_b_okay(mBYE);
	gSIPInterface.writeAsterisk(okay);
	osip_message_free(okay);
	mState = Cleared;
	return mState;
}


SIPState SIPEngine::MTCSendTrying()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	if (mINVITE==NULL) mState=Fail;
	if (mState==Fail) return mState;
	osip_message_t * trying = sip_trying(mINVITE, mSIPUsername.c_str(), mAsteriskIP);
	gSIPInterface.writeAsterisk(trying);
	osip_message_free(trying);
	mState=Proceeding;
	return mState;
}


SIPState SIPEngine::MTCSendRinging()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	assert(mINVITE);

	// Set the configuration for
	// ack message.	
	char tmp[20];
	make_tag(tmp);
	mToTag = tmp;
		
	LOG(DEBUG) << "send ringing";
	osip_message_t * ringing = sip_ringing(mINVITE, 
		mSIPUsername.c_str(), mAsteriskIP, mToTag.c_str());
	gSIPInterface.writeAsterisk(ringing);
	osip_message_free(ringing);

	mState = Proceeding;
	return mState;
}



SIPState SIPEngine::MTCSendOK( short wRTPPort, unsigned wCodec )
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	assert(mINVITE);
	mRTPPort = wRTPPort;
	mCodec = wCodec;
	LOG(DEBUG) << "port=" << wRTPPort << " codec=" << mCodec;
	// Form ack from invite and new parameters.
	osip_message_t * okay = sip_okay(mINVITE, mSIPUsername.c_str(),
		gConfig.getStr("SIP.IP"), mSIPPort, mToTag.c_str() , mRTPPort, mCodec);
	gSIPInterface.writeAsterisk(okay);
	osip_message_free(okay);
	mState=Connecting;
	return mState;
}

SIPState SIPEngine::MTCWaitForACK()
{
	// wait for ack,set this to timeout of 
	// of call channel.  If want a longer timeout 
	// period, need to split into 2 handle situation 
	// like MOC where this fxn if called multiple times. 
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	osip_message_t * ack;

	try {
		// FIXME -- What's the official timeout here?  Is it configurable?
		ack = gSIPInterface.read(mCallID, 1000);
	}
	catch (SIPTimeout& e) {
		LOG(NOTICE) << "timeout";
		mState = Timeout;	
		return mState;
	}
	catch (SIPError& e) {
		LOG(NOTICE) << "read error";
		mState = Fail;
		return mState;
	}

	if (ack->sip_method==NULL) {
		LOG(NOTICE) << "SIP message with no method, status " <<  ack->status_code;
		mState = Fail;
		osip_message_free(ack);
		return mState;	
	}

	LOG(INFO) << "received sip_method="<<ack->sip_method;

	// check for duplicated INVITE
	if( strcmp(ack->sip_method,"INVITE") == 0){ 
		LOG(NOTICE) << "received duplicate INVITE";
	}
	// check for the ACK
	else if( strcmp(ack->sip_method,"ACK") == 0){ 
		LOG(INFO) << "received ACK";
		mState=Active;
	}
	// check for the CANCEL
	else if( strcmp(ack->sip_method,"CANCEL") == 0){ 
		LOG(INFO) << "received CANCEL";
		mState=Fail;
	}
	// check for strays
	else {
		LOG(NOTICE) << "unexpected Message "<<ack->sip_method; 
		mState = Fail;
	}

	osip_message_free(ack);
	return mState;	
}

void SIPEngine::InitRTP(const osip_message_t * msg )
{
	if(session == NULL)
		session = rtp_session_new(RTP_SESSION_SENDRECV);

	rtp_session_set_blocking_mode(session, TRUE);
	rtp_session_set_scheduling_mode(session, TRUE);

	rtp_session_set_connected_mode(session, TRUE);
	rtp_session_set_symmetric_rtp(session, TRUE);
	// Hardcode RTP session type to GSM full rate (GSM 06.10).
	// FIXME -- Make this work for multiple vocoder types.
	rtp_session_set_payload_type(session, 3);

	char d_ip_addr[20];
	char d_port[10];
	get_rtp_params(msg, d_port, d_ip_addr);
	LOG(DEBUG) << "IP="<<d_ip_addr<<" "<<d_port<<" "<<mRTPPort;

	rtp_session_set_local_addr(session, "0.0.0.0", mRTPPort );
	rtp_session_set_remote_addr(session, d_ip_addr, atoi(d_port));

}


void SIPEngine::MTCInitRTP()
{
	assert(mINVITE);
	InitRTP(mINVITE);
}


void SIPEngine::MOCInitRTP()
{
	assert(mOK);
	InitRTP(mOK);
}




void SIPEngine::TxFrame( unsigned char * tx_frame ){
	if(mState!=Active) return;
	else {
		// HACK -- Hardcoded for GSM/8000.
		// FIXME -- Make this work for multiple vocoder types.
		rtp_session_send_with_ts(session, tx_frame, 33, tx_time);
		tx_time += 160;		
	}
}


int SIPEngine::RxFrame(unsigned char * rx_frame){
	if(mState!=Active) return 0; 
	else {
		int more;
		int ret=0;
		// HACK -- Hardcoded for GSM/8000.
		// FIXME -- Make this work for multiple vocoder types.
		ret = rtp_session_recv_with_ts(session, rx_frame, 33, rx_time, &more);
		rx_time += 160;
		return ret;
	}	
}




SIPState SIPEngine::MOSMSSendMESSAGE(const char * wCalledUsername, 
	const char * wCalledDomain , const char *messageText)
{
	LOG(DEBUG) << "mState=" << mState;
	LOG(INFO) << "SIP send to " << wCalledUsername << "@" << wCalledDomain << " MESSAGE " << messageText;
	// Before start, need to add mCallID
	gSIPInterface.addCall(mCallID);
	
	// Set MESSAGE params. 
	// New from tag + via branch
	char tmp[100];
	make_tag(tmp);
	mFromTag = tmp;
	make_branch(tmp);
	mViaBranch = tmp;
	mCSeq++;

	mRemoteUsername = wCalledUsername;
	mRemoteDomain = wCalledDomain;

	osip_message_t * message = sip_message(
		mRemoteUsername.c_str(), mSIPUsername.c_str(), 
		mSIPPort, gConfig.getStr("SIP.IP"), mMessengerIP, 
		mFromTag.c_str(), mViaBranch.c_str(), mCallID.c_str(), mCSeq,
		messageText); 
	
	// Send Invite to Asterisk.
	gSIPInterface.writeMessenger(message);
	osip_message_free(message);
	mState = MessageSubmit;
	return mState;
};


SIPState SIPEngine::MOSMSWaitForSubmit()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;

	try {
		osip_message_t * ok = gSIPInterface.read(mCallID, INVITETimeout);
		// That should never return NULL.
		assert(ok);
		if((ok->status_code==200) || (ok->status_code==202) ) {
			mState = Cleared;
			LOG(INFO) << "successful";
		}
		osip_message_free(ok);
	}

	catch (SIPTimeout& e) {
		LOG(ALARM) << "timed out, is SMS server OK?"; 
		mState = Fail;
	}

	return mState;

}



SIPState SIPEngine::MTSMSSendOK()
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;
	// If this operation was initiated from the CLI, there was no INVITE.
	if (!mINVITE) {
		LOG(INFO) << "clearing CLI-generated transaction";
		mState=Cleared;
		return mState;
	}
	// Form ack from invite and new parameters.
	osip_message_t * okay = sip_okay_SMS(mINVITE, mSIPUsername.c_str(),
		gConfig.getStr("SIP.IP"), mSIPPort, mToTag.c_str());
	gSIPInterface.writeMessenger(okay);
	osip_message_free(okay);
	mState=Cleared;
	return mState;
}



bool SIPEngine::sendINFOAndWaitForOK(unsigned wInfo)
{
	LOG(INFO) << "user " << mSIPUsername << " state " << mState;

	mCSeq++;
	osip_message_t * info = sip_info( wInfo,
		mRemoteUsername.c_str(), mRTPPort, mSIPUsername.c_str(), 
		mSIPPort, gConfig.getStr("SIP.IP"), mAsteriskIP, 
		mFromTag.c_str(), mViaBranch.c_str(), mCallID.c_str(), mCSeq); 
	gSIPInterface.writeAsterisk(info);
	osip_message_free(info);

	try {
		osip_message_t *msg = gSIPInterface.read(mCallID, INVITETimeout);
		return (msg->status_code == 200);
	}
	catch (SIPTimeout& e) { 
		LOG(NOTICE) << "timeout";
		return false;
	}

};




// vim: ts=4 sw=4
