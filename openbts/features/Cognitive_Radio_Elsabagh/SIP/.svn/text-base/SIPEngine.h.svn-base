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


#ifndef SIPENGINE_H
#define SIPENGINE_H

#include <string>
#include <sys/time.h>
#include <sys/types.h>
#include <semaphore.h>

#include <osip2/osip.h>
#include <ortp/ortp.h>


#include "Sockets.h"
#include <Globals.h>


namespace SIP {


class SIPInterface;


enum SIPState  {
	NullState,
	Timeout,
	Starting,
	Proceeding,	
	Ringing,
	Busy,
	Connecting,
	Active,
	Clearing,
	Cleared,
	Fail,
	MessageSubmit
};


/**@name Timeout values for SIP actions, in ms. */
//@{
const unsigned INVITETimeout = 2000;
const unsigned BYETimeout = 2000;
//@}

std::ostream& operator<<(std::ostream& os, SIPState s);

class SIPEngine 
{

public:

	enum Method { SIPRegister =0, SIPUnregister=1 };

private:

	// MOC, MTC information.
	short mRTPPort;
	std::string mRemoteUsername;
	std::string mRemoteDomain;
	unsigned mCodec;

	// General SIP tags and ids.	
	std::string mToTag;
	std::string mFromTag;
	std::string mViaBranch;
	std::string mCallID;
	std::string mSIPUsername;
	unsigned  mCSeq;

	/**@name SIP UDP parameters */
	//@{
	unsigned mSIPPort;
	char mAsteriskIP[256];
	char mMessengerIP[256];
	//@}

	osip_message_t * mINVITE;	///< the INVITE message for this transaction
	osip_message_t * mOK;		///< the INVITE-OK message for this transaction
	osip_message_t * mBYE;		///< the BYE message for this transaction

public:
	
	RtpSession * session;

private:

	SIPState mState;

public:
	
	unsigned int tx_time;
	unsigned int rx_time;
	int time_outs;

	/** Default contructor. Initialize the object. */
	SIPEngine()
		:mCSeq(random()%1000),
		mINVITE(NULL), mOK(NULL), mBYE(NULL),
		session(NULL), mState(NullState),tx_time(0), rx_time(0)
	{
		mSIPPort = gConfig.getNum("SIP.Port");
		const char* wAsteriskIP = gConfig.getStr("Asterisk.IP");
		assert(strlen(wAsteriskIP)<256);
		strcpy(mAsteriskIP,wAsteriskIP);
		const char* wMessengerIP = gConfig.getStr("Smqueue.IP");
		assert(strlen(wMessengerIP)<256);
		strcpy(mMessengerIP,wMessengerIP);
	}


	/** Destroy held message copies. */
	~SIPEngine();

	const std::string& callID() const { return mCallID; } 

	/** Return the current SIP call state. */
	SIPState state() const { return mState; }

	// will automatically allocate mCallID. 
	// good for mobile originated call and registration.
	// IMSI gets prefixed with "IMSI" to form a SIP username
	void User( const char * IMSI );

	// use this for incoming invite message in SIPInterface.
	// IMSI gets prefixed with "IMSI" to form a SIP username
	void User( const char * wCallID, const char * IMSI , const char *origID, const char *origHost);

	/**@name Messages for SIP registration. */
	//@{

	/**
		Send sip register and look at return msg.
		Can throw SIPTimeout().
		@return True on success.
	*/
	bool Register(Method wMethod=SIPRegister);	

	/**
		Send sip unregister and look at return msg.
		Can throw SIPTimeout().
		@return True on success.
	*/
	bool Unregister() { return (Register(SIPUnregister)); };

	//@}

	
	/**@name Messages associated with MOC procedure. */
	//@{

	/**
		Send an invite message.
		@param called_username SIP userid or E.164 address.
		@param called_domain SIP user's domain.
		@param mRTPPort UDP port to use for speech (will use this and next port)
		@param wCodec Code for codec to be used.
		@return New SIP call state.
	*/
	SIPState MOCSendINVITE(const char * called_username,
		const char * called_domain, short mRTPPort, unsigned wCodec);

	SIPState MOCResendINVITE();

	SIPState MOCWaitForOK();

	SIPState MOCSendACK();

	//@}

	/**@name Messages associated with MOSMS procedure. */
	//@{

	/**
		Send an instant message.
		@param called_username SIP userid or E.164 address.
		@param called_domain SIP user's domain.
		@param message_text MESSAGE payload as a C string.
		@return New SIP call state.
	*/
	SIPState MOSMSSendMESSAGE(const char * called_username,
		const char * called_domain, const char *message_text);

	SIPState MOSMSWaitForSubmit();

	SIPState MTSMSSendOK();

	//@}


	/** Save a copy of an INVITE or MESSAGE message in the engine. */
	void saveINVITE(const osip_message_t *invite);

	/** Save a copy of an OK message in the engine. */
	void saveOK(const osip_message_t *OK);

	/** Save a copy of a BYE message in the engine. */
	void saveBYE(const osip_message_t *BYE);


	/**@name Messages associated with MTC procedure. */
	//@{
	SIPState MTCSendTrying();

	SIPState MTCSendRinging();

	SIPState MTCSendOK(short wRTPPort, unsigned wCodec);

	SIPState MTCWaitForACK();
	//@}

	/**@name Messages associated with MTSMS procedure. */
	//@{

	SIPState MTCSendOK();

	//@}



	/**@name Messages for MOD procedure. */
	//@{
	SIPState MODSendBYE();

	SIPState MODResendBYE();

	SIPState MODWaitForOK();
	//@}


	/**@name Messages for MTD procedure. */
	//@{
	SIPState MTDCheckBYE();	

	SIPState MTDSendOK();
	//@}


	void FlushRTP(){
//		flushq(&session->rtp.rq, FLUSHALL);
//		flushq(&session->rtp.tev_rq, FLUSHALL);
	}

	void TxFrame( unsigned char * tx_frame );
	int  RxFrame(unsigned char * rx_frame);

	// We need the host sides RTP information contained
	// in INVITE or 200 OK
	void InitRTP(const osip_message_t * msg );
	void MOCInitRTP();
	void MTCInitRTP();

	/** In-call Signalling */
	//@{

	/**
		Send a SIP INFO message, usually for DTMF.
		Most parameters taken from current SIPEngine state.
		This call blocks for the response.
		@param wInfo The DTMF signalling code.
		@return Success/Fail flag.
	*/
	bool sendINFOAndWaitForOK(unsigned wInfo);

	//@}

};


}; 

#endif // SIPENGINE_H
// vim: ts=4 sw=4
