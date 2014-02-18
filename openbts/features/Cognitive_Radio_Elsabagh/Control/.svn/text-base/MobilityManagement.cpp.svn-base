/**@file GSM/SIP Mobility Management, GSM 04.08. */
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


#include "Timeval.h"

#include "ControlCommon.h"
#include "GSMLogicalChannel.h"
#include "GSML3RRMessages.h"
#include "GSML3MMMessages.h"
#include "GSML3CCMessages.h"
#include "GSMConfig.h"

#include "CollectMSInfo.h"

using namespace std;

#include "SIPInterface.h"
#include "SIPUtility.h"
#include "SIPMessage.h"
#include "SIPEngine.h"
using namespace SIP;


using namespace GSM;
using namespace Control;


/** Controller for CM Service requests, dispatches out to multiple possible transaction controllers. */
void Control::CMServiceResponder(const L3CMServiceRequest* cmsrq, LogicalChannel* DCCH)
{
	assert(cmsrq);
	assert(DCCH);
	LOG(INFO) << *cmsrq;
	switch (cmsrq->serviceType().type()) {
		case L3CMServiceType::MobileOriginatedCall:
			MOCStarter(cmsrq,DCCH);
			break;
		case L3CMServiceType::ShortMessage:
			MOSMSController(cmsrq,DCCH);
			break;
		case L3CMServiceType::EmergencyCall:
			EmergencyCall(cmsrq,DCCH);
			break;
		default:
			LOG(NOTICE) << "service not supported for " << *cmsrq;
			// Cause 0x20 means "serivce not supported".
			DCCH->send(L3CMServiceReject(0x20));
			DCCH->send(L3ChannelRelease());
	}
	// The transaction may or may not be cleared,
	// depending on the assignment type.
}




/** Controller for the IMSI Detach transaction, GSM 04.08 4.3.4. */
void Control::IMSIDetachController(const L3IMSIDetachIndication* idi, LogicalChannel* DCCH)
{
	assert(idi);
	assert(DCCH);
	LOG(INFO) << *idi;

	// The IMSI detach maps to a SIP unregister with the local Asterisk server.
	try { 
		// FIXME -- Resolve TMSIs to IMSIs.
		if (idi->mobileIdentity().type()==IMSIType) {
			SIPEngine engine;
			engine.User(idi->mobileIdentity().digits());
			engine.Unregister();
		}
	}
	catch(SIPTimeout) {
		LOG(ALARM) "SIP registration timed out.  Is Asterisk running?";
	}
	// No reponse required, so just close the channel.
	DCCH->send(L3ChannelRelease());
	// Many handsets never complete the transaction.
	// So force a shutdown of the channel.
	DCCH->send(HARDRELEASE);
}




/**
	Send a given welcome message from a given short code.
	@return true if it was sent
*/
bool sendWelcomeMessage(const char* messageName, const char* shortCodeName, const char *IMSI, SDCCHLogicalChannel* SDCCH)
{
	if (!gConfig.defines(messageName)) return false;
	LOG(INFO) << "sending " << messageName << " message to handset";
	ostringstream message;
	message << gConfig.getStr(messageName) << " IMSI:" << IMSI;
	// This returns when delivery is acked in L3.
	deliverSMSToMS(
		gConfig.getStr(shortCodeName),
		message.str().c_str(),
		random()%7,SDCCH);
	return true;
}


/**
	Controller for the Location Updating transaction, GSM 04.08 4.4.4.
	@param lur The location updating request.
	@param SDCCH The Dm channel to the MS, which will be released by the function.
*/
void Control::LocationUpdatingController(const L3LocationUpdatingRequest* lur, SDCCHLogicalChannel* SDCCH)
{
	assert(SDCCH);
	assert(lur);
	LOG(INFO) << *lur;

	// The location updating request gets mapped to a SIP
	// registration with the Asterisk server.
	// If the registration is successful, we may assign a new TMSI.

	// Resolve an IMSI and see if there's a pre-existing IMSI-TMSI mapping.
	// This operation will throw an exception, caught in a higher scope,
	// if it fails in the GSM domain.
	L3MobileIdentity mobID = lur->mobileIdentity();
	bool sameLAI = (lur->LAI() == gBTS.LAI());
	unsigned preexistingTMSI = resolveIMSI(sameLAI,mobID,SDCCH);
	// IMSIAttach set to true if this is a new registration.
	bool IMSIAttach = (preexistingTMSI==0);
	// Try to register the IMSI with Asterisk.
	// This will be set true if registration succeeded in the SIP world.
	bool success = false;
	try {
		SIPEngine engine;
		engine.User(mobID.digits());
		LOG(DEBUG) << "waiting for registration";
		success = engine.Register(); 
	}
	catch(SIPTimeout) {
		LOG(ALARM) "SIP registration timed out.  Is Asterisk running?";
		// Reject with a "network failure" cause code, 0x11.
		SDCCH->send(L3LocationUpdatingReject(0x11));
		// HACK -- wait long enough for a response
		// FIXME -- Why are we doing this?
		sleep(4);
		// Release the channel and return.
		SDCCH->send(L3ChannelRelease());
		return;
	}

	// This allows us to configure Open Registration
	bool openRegistration = gConfig.defines("Control.OpenRegistration");

	// Do we need to assign a TMSI?
	unsigned newTMSI = 0;
	if (!preexistingTMSI && (
			gConfig.defines("Control.LUR.TMSIsAll") ||
			success ||
			openRegistration
		) ) {
			newTMSI = gTMSITable.assign(mobID.digits());
	}


	// Query for IMEI?
	if (IMSIAttach && gConfig.defines("Control.LUR.QueryIMEI")) {
		SDCCH->send(L3IdentityRequest(IMEIType));
		L3Message* msg = getMessage(SDCCH);
		L3IdentityResponse *resp = dynamic_cast<L3IdentityResponse*>(msg);
		if (!resp) {
			if (msg) {
				LOG(WARN) << "Unexpected message " << *msg;
				delete msg;
			}
			throw UnexpectedMessage();
		}
		LOG(INFO) << *resp;
	}

	// Query for classmark?
	if (IMSIAttach && gConfig.defines("Control.LUR.QueryClassmark")) {
		SDCCH->send(L3ClassmarkEnquiry());
		L3Message* msg = getMessage(SDCCH);
		L3ClassmarkChange *resp = dynamic_cast<L3ClassmarkChange*>(msg);
		if (!resp) {
			if (msg) {
				LOG(WARN) << "Unexpected message " << *msg;
				delete msg;
			}
			throw UnexpectedMessage();
		}
		LOG(INFO) << *resp;
	}

	// Query for RRLP?
	GSM::RRLP::collectMSInfo(mobID, SDCCH, gConfig.defines("GSM.RRLP.LUR"));

	// We fail closed unless we're configured otherwise
	if (!success && !openRegistration) {
		LOG(INFO) << "registration FAILED: " << mobID;
		SDCCH->send(L3LocationUpdatingReject(gConfig.getNum("GSM.LURejectCause")));
		if (!preexistingTMSI) {
			sendWelcomeMessage( "Control.FailedRegistrationWelcomeMessage",
				"Control.FailedRegistrationWelcomeShortCode", mobID.digits(),SDCCH);
		}
		// Release the channel and return.
		SDCCH->send(L3ChannelRelease());
		return;
	}

	// If success is true, we had a normal registration.
	// Otherwise, we are here because of open registration.
	// Either way, we're going to register a phone if we arrive here.

	if (success) LOG(INFO) << "registration SUCCESS: " << mobID;
	else LOG(INFO) << "registration ALLOWED: " << mobID;


	// Send the "short name".
	if (gConfig.defines("GSM.ShortName"))
		SDCCH->send(L3MMInformation(gConfig.getStr("GSM.ShortName")));
	// Accept. Make a TMSI assignment, too, if needed.
	if (preexistingTMSI) {
		SDCCH->send(L3LocationUpdatingAccept(gBTS.LAI()));
	} else {
		assert(newTMSI);
		SDCCH->send(L3LocationUpdatingAccept(gBTS.LAI(),newTMSI));
		L3Frame* resp = SDCCH->recv(1000); // wait for the MM TMSI REALLOCATION COMPLETE message
		if (!resp) {
			LOG(NOTICE) << "no response to TMSI assignment";
		} else {
			LOG(INFO) << *resp;
		}
		delete resp;
	}

	// If this is an IMSI attach, send a welcome message.
	if (IMSIAttach) {
		if (success) {
			sendWelcomeMessage( "Control.NormalRegistrationWelcomeMessage",
				"Control.NormalRegistrationWelcomeShortCode", mobID.digits(), SDCCH);
		} else {
			sendWelcomeMessage( "Control.OpenRegistrationWelcomeMessage",
				"Control.OpenRegistrationWelcomeShortCode", mobID.digits(), SDCCH);
		}
	}

	// Release the channel and return.
	SDCCH->send(L3ChannelRelease());
	return;
}




// vim: ts=4 sw=4
