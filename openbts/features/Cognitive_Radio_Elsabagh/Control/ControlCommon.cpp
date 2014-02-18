/**@file Common-use functions for the control layer. */

/*
* Copyright 2008, 2010 Free Software Foundation, Inc.
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
#include <GSML3Message.h>
#include <GSML3CCMessages.h>
#include <GSML3RRMessages.h>
#include <GSML3MMMessages.h>
#include <GSMConfig.h>

#include <SIPEngine.h>
#include <SIPInterface.h>


using namespace std;
using namespace GSM;
using namespace Control;



// The global transaction table.
TransactionTable gTransactionTable;

// The global TMSI table.
TMSITable gTMSITable;





TransactionEntry::TransactionEntry()
	:mID(gTransactionTable.newID()),
	mQ931State(NullState),
	mT301(T301ms), mT302(T302ms), mT303(T303ms),
	mT304(T304ms), mT305(T305ms), mT308(T308ms),
	mT310(T310ms), mT313(T313ms),
	mT3113(gConfig.getNum("GSM.T3113")),
	mTR1M(TR1Mms)
{
	mMessage[0]='\0';
}

// Form for MT transactions.
TransactionEntry::TransactionEntry(const L3MobileIdentity& wSubscriber, 
	const L3CMServiceType& wService,
	const L3CallingPartyBCDNumber& wCalling,
	const char *wMessage)
	:mID(gTransactionTable.newID()),
	mSubscriber(wSubscriber),mService(wService),
	mTIFlag(1), mTIValue(0),
	mCalling(wCalling),
	mQ931State(NullState),
	mT301(T301ms), mT302(T302ms), mT303(T303ms),
	mT304(T304ms), mT305(T305ms), mT308(T308ms),
	mT310(T310ms), mT313(T313ms),
	mT3113(gConfig.getNum("GSM.T3113")),
	mTR1M(TR1Mms)
{
	if (wMessage) strncpy(mMessage,wMessage,160);
	else mMessage[0]='\0';
}

// Form for MO transactions.
TransactionEntry::TransactionEntry(const L3MobileIdentity& wSubscriber,
	const L3CMServiceType& wService,
	unsigned wTIValue,
	const L3CalledPartyBCDNumber& wCalled)
	:mID(gTransactionTable.newID()),
	mSubscriber(wSubscriber),mService(wService),
	mTIFlag(0), mTIValue(wTIValue),
	mCalled(wCalled),
	mQ931State(NullState),
	mT301(T301ms), mT302(T302ms), mT303(T303ms),
	mT304(T304ms), mT305(T305ms), mT308(T308ms),
	mT310(T310ms), mT313(T313ms),
	mT3113(gConfig.getNum("GSM.T3113")),
	mTR1M(TR1Mms)
{
	mMessage[0]='\0';
}

// Form for MT transactions.
TransactionEntry::TransactionEntry(const L3MobileIdentity& wSubscriber,
	const L3CMServiceType& wService,
	unsigned wTIValue,
	const L3CallingPartyBCDNumber& wCalling)
	:mID(gTransactionTable.newID()),
	mSubscriber(wSubscriber),mService(wService),
	mTIFlag(1),mTIValue(wTIValue),mCalling(wCalling),
	mQ931State(NullState),
	mT301(T301ms), mT302(T302ms), mT303(T303ms),
	mT304(T304ms), mT305(T305ms), mT308(T308ms),
	mT310(T310ms), mT313(T313ms),
	mT3113(gConfig.getNum("GSM.T3113")),
	mTR1M(TR1Mms)
{
	mMessage[0]='\0';
}



bool TransactionEntry::timerExpired() const
{
	// FIXME -- If we were smart, this would be a table.
	if (mT301.expired()) {
		OBJLOG(DEBUG) << "T301 expired";
		return true;
	}
	if (mT302.expired()) {
		OBJLOG(DEBUG) << "T302 expired";
		return true;
	}
	if (mT303.expired()) {
		OBJLOG(DEBUG) << "T303 expired";
		return true;
	}
	if (mT304.expired()) {
		OBJLOG(DEBUG) << "T304 expired";
		return true;
	}
	if (mT305.expired()) {
		OBJLOG(DEBUG) << "T305 expired";
		return true;
	}
	if (mT308.expired()) {
		OBJLOG(DEBUG) << "T308 expired";
		return true;
	}
	if (mT310.expired()) {
		OBJLOG(DEBUG) << "T310 expired";
		return true;
	}
	if (mT313.expired()) {
		OBJLOG(DEBUG) << "T313 expired";
		return true;
	}
	if (mTR1M.expired()) {
		OBJLOG(DEBUG) << " TR1M expired";
		return true;
	}
	return false;
}


void TransactionEntry::resetTimers()
{
	mT301.reset();
	mT302.reset();
	mT303.reset();
	mT304.reset();
	mT305.reset();
	mT308.reset();
	mT310.reset();
	mT313.reset();
	mTR1M.reset();
}



bool TransactionEntry::dead() const
{
	if (mQ931State==NullState) return true;
	if ((mQ931State==Paging) && mT3113.expired()) return true;
	return false;
}


ostream& Control::operator<<(ostream& os, TransactionEntry::Q931CallState state)
{
	switch (state) {
		case TransactionEntry::NullState: os << "null"; break;
		case TransactionEntry::Paging: os << "paging"; break;
		case TransactionEntry::MOCInitiated: os << "MOC initiated"; break;
		case TransactionEntry::MOCProceeding: os << "MOC proceeding"; break;
		case TransactionEntry::MTCConfirmed: os << "MTC confirmed"; break;
		case TransactionEntry::CallReceived: os << "call received"; break;
		case TransactionEntry::CallPresent: os << "call present"; break;
		case TransactionEntry::ConnectIndication: os << "connect indication"; break;
		case TransactionEntry::Active: os << "active"; break;
		case TransactionEntry::DisconnectIndication: os << "disconnect indication"; break;
		case TransactionEntry::ReleaseRequest: os << "release request"; break;
		case TransactionEntry::SMSDelivering: os << "SMS delivery"; break;
		case TransactionEntry::SMSSubmitting: os << "SMS submission"; break;
		default: os << "?" << (int)state << "?";
	}
	return os;
}

ostream& Control::operator<<(ostream& os, const TransactionEntry& entry)
{
	os << entry.ID();
	os << " TI=(" << entry.TIFlag() << "," << entry.TIValue() << ") ";
	os << entry.subscriber();
	os << " " << entry.service();
	if (entry.called().digits()[0]) os << " to=" << entry.called().digits();
	if (entry.calling().digits()[0]) os << " from=" << entry.calling().digits();
	os << " Q.931State=" << entry.Q931State();
	os << " SIPState=" << entry.SIP().state();
	os << " (" << (entry.stateAge()+500)/1000 << " sec)";
	if (entry.message()[0]) os << " message=\"" << entry.message() << "\"";
	return os;
}


unsigned TransactionTable::newID()
{
	mLock.lock();
	unsigned ID = mIDCounter++;
	mLock.unlock();
	return ID;
}


void TransactionTable::add(const TransactionEntry& value)
{
	LOG(INFO) << "new transaction " << value;
	mLock.lock();
	mTable[value.ID()]=value;
	mLock.unlock();
}


void TransactionTable::update(const TransactionEntry& value)
{
	// ID==0 is a non-valid special case.
	assert(value.ID());
	mLock.lock();
	if (mTable.find(value.ID())==mTable.end()) {
		mLock.unlock();
		LOG(WARN) << "attempt to update non-existent transaction entry with key " << value.ID();
		return;
	}
	mTable[value.ID()]=value;
	mLock.unlock();
}




bool TransactionTable::find(unsigned key, TransactionEntry& target)
{
	// ID==0 is a non-valid special case.
	assert(key);
	mLock.lock();
	TransactionMap::iterator itr = mTable.find(key);
	if (itr==mTable.end()) {
		mLock.unlock();
		return false;
	}
	if (itr->second.dead()) {
		mTable.erase(itr);
		mLock.unlock();
		return false;
	}
	mLock.unlock();
	target = itr->second;
	return true;
}



bool TransactionTable::remove(unsigned key)
{
	// ID==0 is a non-valid special case.
	assert(key);
	mLock.lock();
	bool retVal = mTable.erase(key);
	mLock.unlock();
	return retVal;
}



void TransactionTable::clearDeadEntries()
{
	mLock.lock();
	TransactionMap::iterator itr = mTable.begin();
	while (itr!=mTable.end()) {
		if (!itr->second.dead()) ++itr;
		else {
			LOG(DEBUG) << "erasing " << itr->first;
			TransactionMap::iterator old = itr;
			itr++;
			mTable.erase(old);
		}
	}
	mLock.unlock();
}



bool TransactionTable::find(const L3MobileIdentity& mobileID, TransactionEntry& target)
{
	// Yes, it's linear time.
	// Even in a 6-ARFCN system, it should rarely be more than a dozen entries.

	// Since clearDeadEntries is also linear, do that here, too.

	// Brtue force search.
	bool foundIt = false;
	mLock.lock();
	clearDeadEntries();
	TransactionMap::const_iterator itr = mTable.begin();
	while (itr!=mTable.end()) {
		const TransactionEntry& transaction = itr->second;
		if (transaction.subscriber()==mobileID) {
			// No need to check dead(), since we just cleared the table.
			foundIt = true;
			target = transaction;
			break;
		}
		++itr;
	}
	mLock.unlock();
	return foundIt;
}

size_t TransactionTable::size()
{
	return mTable.size();
}


void Control::clearTransactionHistory( TransactionEntry& transaction )
{
	SIP::SIPEngine& engine = transaction.SIP();
	LOG(DEBUG) << engine.callID()<<" "<< transaction.ID();
	gSIPInterface.removeCall(engine.callID());
	gTransactionTable.remove(transaction.ID());
}


void Control::clearTransactionHistory(unsigned transactionID)
{
	if (transactionID==0) return;
	TransactionEntry transaction;
	if (gTransactionTable.find(transactionID,transaction)) {
		clearTransactionHistory(transaction);
	} else {
		LOG(INFO) << "clearTransactionHistory didn't find " << transactionID << "(size = " << gTransactionTable.size() << ")";
	}
}





void TMSIRecord::save(unsigned TMSI, FILE* fp) const
{
	fprintf(fp, "%10u %10u %10u %15s %15s\n", TMSI, mCreated.sec(), mTouched.sec(), mIMSI.c_str(), mIMEI.c_str());
}


unsigned TMSIRecord::load(FILE* fp)
{
	unsigned TMSI=0;
	unsigned created, touched;
	char IMSI[16];
	char IMEI[16];
	fscanf(fp, "%10u %10u %10u %15s %15s\n", &TMSI, &created, &touched, IMSI, IMEI);
	if (created>touched) {
		LOG(ALARM) << "corrupt TMSI file";
		return 0;
	}
	mIMSI = IMSI;
	mIMEI = IMEI;
	mTouched = Timeval(touched,0);
	mCreated = Timeval(created,0);
	return TMSI;
}



unsigned TMSITable::assign(const char* IMSI)
{
	purge();
	mLock.lock();
	// Is this IMSI already in here?
	unsigned oldTMSI = TMSI(IMSI);
	if (oldTMSI) {
		mLock.unlock();
		return oldTMSI;
	}
	unsigned TMSI = mCounter++;
	mMap[TMSI] = TMSIRecord(IMSI);
	mLock.unlock();
	if (gConfig.defines("Control.TMSITable.SavePath")) save(gConfig.getStr("Control.TMSITable.SavePath"));
	return TMSI;
}


bool TMSITable::find(unsigned TMSI, TMSIRecord& target)
{
	mLock.lock();
	TMSIMap::iterator iter = mMap.find(TMSI);
	if (iter==mMap.end()) {
		mLock.unlock();
		return false;
	}
	target = iter->second;
	// Is it too old?
	if (target.age() > 3600*gConfig.getNum("Control.TMSITable.MaxAge")) {
		mMap.erase(iter);
		mLock.unlock();
		return false;
	}
	iter->second.touch();
	mLock.unlock();
	return true;
}

const char* TMSITable::IMSI(unsigned TMSI) const
{
	mLock.lock();
	TMSIMap::const_iterator iter = mMap.find(TMSI);
	mLock.unlock();
	if (iter==mMap.end()) return NULL;
	iter->second.touch();
	return iter->second.IMSI();
}

unsigned TMSITable::TMSI(const char* IMSI) const
{
	// FIXME -- If we were smart, we'd organize the table for a log-time search.
	unsigned TMSI = 0;
	string IMSIc(IMSI);
	mLock.lock();
	// brute force search
	TMSIMap::const_iterator itr = mMap.begin();
	while (itr!=mMap.end()) {
		if (itr->second.IMSI() == IMSIc) {
			TMSI = itr->first;
			itr->second.touch();
			break;
		}
		++itr;
	}
	mLock.unlock();
	return TMSI;
}




void TMSITable::erase(unsigned TMSI)
{
	mLock.lock();
	TMSIMap::iterator iter = mMap.find(TMSI);
	if (iter!=mMap.end()) mMap.erase(iter);
	mLock.unlock();
}




size_t TMSITable::size() const {
	mLock.lock();
	size_t retVal = mMap.size();
	mLock.unlock();
	return retVal;
}


void TMSITable::purge()
{
	mLock.lock();
	// We rely on the fact the TMSIs are assigned in numeric order
	// to erase the oldest first.
	while ( (mMap.size()>gConfig.getNum("Control.TMSITable.MaxSize")) && (mClear!=mCounter) )
		mMap.erase(mClear++);
	mLock.unlock();
}



size_t printAge(unsigned seconds, char *buf)
{
	static const unsigned k=5;
	if (seconds<k*60) return sprintf(buf,"%4us",seconds);
	unsigned minutes = (seconds+30) / 60;
	if (minutes<k*60) return sprintf(buf,"%4um",minutes);
	unsigned hours = (minutes+30) / 60;
	if (hours<k*24) return sprintf(buf,"%4uh", hours);
	return sprintf(buf,"%4ud",(hours+12)/24);
}

ostream& Control::operator<<(ostream& os, const TMSIRecord& rec)
{
	char buf[100];
	char ageStr[10];
	printAge(rec.age(),ageStr);
	char touchedStr[10];
	printAge(rec.touched(),touchedStr);
	sprintf(buf,"%15s %15s %s %s", rec.IMSI(), rec.IMEI(), ageStr, touchedStr);
	os << buf;
	return os;
}


void TMSITable::dump(ostream& os) const
{
	mLock.lock();
	TMSIMap::const_iterator tp = mMap.begin();
	while (tp != mMap.end()) {
		os << hex << "0x" << tp->first << " " << dec << tp->second << endl;
		++tp;
	}
	mLock.unlock();
}


void TMSITable::save(const char* filename) const
{
	FILE* fp = fopen(filename,"w");
	if (!fp) {
		LOG(ALARM) << "TMSITable cannot open " << filename << " for writing";
		return;
	}
	LOG(INFO) << "saving TMSI table to " << filename;
	mLock.lock();
	TMSIMap::const_iterator tp = mMap.begin();
	while (tp != mMap.end()) {
		tp->second.save(tp->first,fp);
		++tp;
	}
	mLock.unlock();
	fclose(fp);
}

void TMSITable::load(const char* filename)
{
	FILE* fp = fopen(filename,"r");
	if (!fp) {
		LOG(ALARM) << "TMSITable cannot open " << filename << " for reading";
		return;
	}
	LOG(INFO) << "loading TMSI table from " << filename;
	mLock.lock();
	while (!feof(fp)) {
		TMSIRecord val;
		unsigned key = val.load(fp);
		if (!key) break;
		mMap[key] = val;
	}
	mLock.unlock();
	fclose(fp);
}




bool Control::waitForPrimitive(LogicalChannel *LCH, Primitive primitive, unsigned timeout_ms)
{
	bool waiting = true;
	while (waiting) {
		L3Frame *req = LCH->recv(timeout_ms);
		if (req==NULL) {
			LOG(NOTICE) << "timeout at uptime " << gBTS.uptime() << " frame " << gBTS.time();
			return false;
		}
		waiting = (req->primitive()!=primitive);
		delete req;
	}
	return true;
}


void Control::waitForPrimitive(LogicalChannel *LCH, Primitive primitive)
{
	bool waiting = true;
	while (waiting) {
		L3Frame *req = LCH->recv();
		if (req==NULL) continue;
		waiting = (req->primitive()!=primitive);
		delete req;
	}
}




// FIXME -- getMessage should return an L3Frame, not an L3Message.
// This will mean moving all of the parsing into the control layer.
// FIXME -- This needs an adjustable timeout.

L3Message* Control::getMessage(LogicalChannel *LCH, unsigned SAPI)
{
	while (1) {
		unsigned timeout_ms = LCH->N200() * T200ms;
		L3Frame *rcv = LCH->recv(timeout_ms,SAPI);
		if (rcv==NULL) {
			LOG(NOTICE) << "timeout";
			throw ChannelReadTimeout();
		}
		LOG(DEBUG) << "received " << *rcv;
		Primitive primitive = rcv->primitive();
		if (primitive!=DATA) {
			LOG(NOTICE) << "unexpected primitive " << primitive;
			delete rcv;
			throw UnexpectedPrimitive();
		}
		// If we get an unparsable message, ignore it and try again.
		L3Message *msg = parseL3(*rcv);
		delete rcv;
		if (msg) return msg;
	}
}






/* Resolve a mobile ID to an IMSI and return TMSI if it is assigned. */
unsigned  Control::resolveIMSI(bool sameLAI, L3MobileIdentity& mobID, LogicalChannel* LCH)
{
	// Returns known or assigned TMSI.
	assert(LCH);
	LOG(DEBUG) << "resolving mobile ID " << mobID << ", sameLAI: " << sameLAI;

	// IMSI already?  See if there's a TMSI already, too.
	// This is a linear time operation, but should only happen on
	// the first registration by this mobile.
	if (mobID.type()==IMSIType) return gTMSITable.TMSI(mobID.digits());

	// IMEI?  WTF?!
	// FIXME -- Should send MM Reject, cause 0x60, "invalid mandatory information".
	if (mobID.type()==IMEIType) throw UnexpectedMessage();

	// Must be a TMSI.
	// Look in the table to see if it's one we assigned.
	unsigned TMSI = mobID.TMSI();
	const char* IMSI = NULL;
	if (sameLAI) IMSI = gTMSITable.IMSI(TMSI);
	if (IMSI) {
		// We assigned this TMSI and the TMSI/IMSI pair is already in the table.
		mobID = L3MobileIdentity(IMSI);
		LOG(DEBUG) << "resolving mobile ID (table): " << mobID;
		return TMSI;
	}
	// Not our TMSI.
	// Phones are not supposed to do this, but many will.
	// If the IMSI's not in the table, ASK for it.
	LCH->send(L3IdentityRequest(IMSIType));
	// FIXME -- This request times out on T3260, 12 sec.  See GSM 04.08 Table 11.2.
	L3Message* msg = getMessage(LCH);
	L3IdentityResponse *resp = dynamic_cast<L3IdentityResponse*>(msg);
	if (!resp) {
		if (msg) delete msg;
		throw UnexpectedMessage();
	}
	mobID = resp->mobileID();
	LOG(INFO) << resp;
	delete msg;
	LOG(DEBUG) << "resolving mobile ID (requested): " << mobID;
	// FIXME -- Should send MM Reject, cause 0x60, "invalid mandatory information".
	if (mobID.type()!=IMSIType) throw UnexpectedMessage();
	// Return 0 to indicate that we have not yet assigned our own TMSI for this phone.
	return 0;
}



/* Resolve a mobile ID to an IMSI. */
void  Control::resolveIMSI(L3MobileIdentity& mobileIdentity, LogicalChannel* LCH)
{
	// Are we done already?
	if (mobileIdentity.type()==IMSIType) return;

	// If we got a TMSI, find the IMSI.
	if (mobileIdentity.type()==TMSIType) {
		const char *IMSI = gTMSITable.IMSI(mobileIdentity.TMSI());
		if (IMSI) mobileIdentity = L3MobileIdentity(IMSI);
	}

	// Still no IMSI?  Ask for one.
	if (mobileIdentity.type()!=IMSIType) {
		LOG(NOTICE) << "MOC with no IMSI or valid TMSI.  Reqesting IMSI.";
		LCH->send(L3IdentityRequest(IMSIType));
		// FIXME -- This request times out on T3260, 12 sec.  See GSM 04.08 Table 11.2.
		L3Message* msg = getMessage(LCH);
		L3IdentityResponse *resp = dynamic_cast<L3IdentityResponse*>(msg);
		if (!resp) {
			if (msg) delete msg;
			throw UnexpectedMessage();
		}
		mobileIdentity = resp->mobileID();
		delete msg;
	}

	// Still no IMSI??
	if (mobileIdentity.type()!=IMSIType) {
		// FIXME -- This is quick-and-dirty, not following GSM 04.08 5.
		LOG(WARN) << "MOC setup with no IMSI";
		// Cause 0x60 "Invalid mandatory information"
		LCH->send(L3CMServiceReject(L3RejectCause(0x60)));
		LCH->send(L3ChannelRelease());
		// The SIP side and transaction record don't exist yet.
		// So we're done.
		return;
	}
}




// vim: ts=4 sw=4
