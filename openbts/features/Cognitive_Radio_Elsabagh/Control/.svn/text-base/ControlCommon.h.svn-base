/**@file Declarations for all externally-visible control-layer functions. */
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



#ifndef CONTROLCOMMON_H
#define CONTROLCOMMON_H


#include <stdio.h>
#include <list>

#include <Logger.h>
#include <Interthread.h>
#include <Timeval.h>


#include <GSML3CommonElements.h>
#include <GSML3MMElements.h>
#include <GSML3CCElements.h>
#include <GSML3RRMessages.h>
#include <SIPEngine.h>


// Enough forward refs to prevent "kitchen sick" includes and circularity.

namespace GSM {
class Time;
class L3Message;
class GSMConfig;
class LogicalChannel;
class SDCCHLogicalChannel;
class CCCHLogicalChannel;
class TCHFACCHLogicalChannel;
class L3Cause;
class L3CMServiceRequest;
class L3LocationUpdatingRequest;
class L3IMSIDetachIndication;
class L3PagingResponse;
};


/**@namespace Control This namepace is for use by the control layer. */
namespace Control {

class TransactionEntry;
class TransactionTable;

/**@name Call control time-out values (in ms) from ITU-T Q.931 Table 9-1 and GSM 04.08 Table 11.4. */
//@{
#ifndef RACETEST
const unsigned T301ms=60000;		///< recv ALERT --> recv CONN
const unsigned T302ms=12000;		///< send SETUP ACK --> any progress
const unsigned T303ms=10000;		///< send SETUP --> recv CALL CONF or REL COMP
const unsigned T304ms=20000;		///< recv SETUP ACK --> any progress
const unsigned T305ms=30000;		///< send DISC --> recv REL or DISC
const unsigned T308ms=30000;		///< send REL --> rev REL or REL COMP
const unsigned T310ms=30000;		///< recv CALL CONF --> recv ALERT, CONN, or DISC
const unsigned T313ms=30000;		///< send CONNECT --> recv CONNECT ACK
#else
// These are reduced values to force testing of poor network behavior.
const unsigned T301ms=18000;		///< recv ALERT --> recv CONN
const unsigned T302ms=1200;		///< send SETUP ACK --> any progress
const unsigned T303ms=400;			///< send SETUP --> recv CALL CONF or REL COMP
const unsigned T304ms=2000;		///< recv SETUP ACK --> any progress
const unsigned T305ms=3000;		///< send DISC --> recv REL or DISC
const unsigned T308ms=3000;		///< send REL --> rev REL or REL COMP
const unsigned T310ms=3000;		///< recv CALL CONF --> recv ALERT, CONN, or DISC
const unsigned T313ms=3000;		///< send CONNECT --> recv CONNECT ACK
#endif
//@}


/**@name Common-use functions from the control layer. */
//@{

/**
	Common-use function to block on a channel until a given primitive arrives.
	Any payload is discarded.
	@param LCH The logcial channel.
	@param primitive The primitive to wait for.
	@param timeout_ms The timeout in milliseconds.
	@return True on success, false on timeout.
*/
bool waitForPrimitive(GSM::LogicalChannel *LCH,
	GSM::Primitive primitive,
	unsigned timeout_ms);

/**
	Common-use function to block on a channel until a given primitive arrives.
	Any payload is discarded.  Block indefinitely, no timeout.
	@param LCH The logcial channel.
	@param primitive The primitive to wait for.
*/
void waitForPrimitive(GSM::LogicalChannel *LCH,
	GSM::Primitive primitive);

/**
	Get a message from a LogicalChannel.
	Close the channel with abnormal release on timeout.
	Caller must delete the returned pointer.
	Throws ChannelReadTimeout, UnexpecedPrimitive or UnsupportedMessage on timeout.
	@param LCH The channel to receive on.
	@param SAPI The service access point.
	@return Pointer to message.
*/
// FIXME -- This needs an adjustable timeout.
GSM::L3Message* getMessage(GSM::LogicalChannel* LCH, unsigned SAPI=0);

/**
	Clear the state information associated with a TransactionEntry.
	Removes the transaction data from both gSIPInterfaceMap and gTransactionTable.
	@param transaction The transaction to clear.
*/
void clearTransactionHistory(TransactionEntry& transaction);

/**
	Clear the state information associated with a TransactionEntry.
	Removes the transaction data from both gSIPInterfaceMap and gTransactionTable.
	@param transactionID The ID of the transaction.
*/
void clearTransactionHistory(unsigned transactionID);

//@}



/**@name Functions for mobility manangement operations. */
//@{
void CMServiceResponder(const GSM::L3CMServiceRequest* cmsrq, GSM::LogicalChannel* DCCH);
void IMSIDetachController(const GSM::L3IMSIDetachIndication* idi, GSM::LogicalChannel* DCCH);
void LocationUpdatingController(const GSM::L3LocationUpdatingRequest* lur, GSM::SDCCHLogicalChannel* SDCCH);
//@}

/**@name Functions for radio resource operations. */
//@{
/** Decode RACH bits and send an immediate assignment. */
void AccessGrantResponder(
	unsigned requestReference, const GSM::Time& when,
	float RSSI, float timingError);
/** Find and compelte the in-process transaction associated with a paging repsonse. */
void PagingResponseHandler(const GSM::L3PagingResponse*, GSM::LogicalChannel*);
/** Find and compelte the in-process transaction associated with a completed assignment. */
void AssignmentCompleteHandler(const GSM::L3AssignmentComplete*, GSM::TCHFACCHLogicalChannel*);
//@}

/**@name Functions for call control operations. */
//@{
/**@name MOC */
//@{
/** Run the MOC to the point of alerting, doing early assignment if needed. */
void MOCStarter(const GSM::L3CMServiceRequest*, GSM::LogicalChannel*);
/** Complete the MOC connection. */
void MOCController(TransactionEntry&, GSM::TCHFACCHLogicalChannel*);
/** Set up an emergency call, assuming very early assignment. */
void EmergencyCall(const GSM::L3CMServiceRequest*, GSM::LogicalChannel*);
//@}
/**@name MTC */
//@{
/** Run the MTC to the point of alerting, doing early assignment if needed. */
void MTCStarter(TransactionEntry&, GSM::LogicalChannel*);
/** Complete the MTC connection. */
void MTCController(TransactionEntry&, GSM::TCHFACCHLogicalChannel*);
//@}
/**@name Test Call */
//@{
/** Run the test call. */
void TestCall(TransactionEntry&, GSM::LogicalChannel*);
//@}
/**@name SMS */
//@{

/** MOSMS state machine.  */
void MOSMSController(const GSM::L3CMServiceRequest *req, 
						GSM::LogicalChannel *LCH);
/**
	Basic SMS delivery from an established CM.
	On exit, SAP3 will be in ABM and LCH will still be open.
	Throws exception for failures in connection layer or for parsing failure.
	@return true on success in relay layer.
*/
bool deliverSMSToMS(const char *callingPartyDigits, const char* message, unsigned TI, GSM::LogicalChannel *LCH);

/** MTSMS */
void MTSMSController(TransactionEntry& transaction, 
						GSM::LogicalChannel *LCH);
//@}

/** Create a new transaction entry and start paging. */
void initiateMTTransaction(TransactionEntry& transaction,
		GSM::ChannelType chanType, unsigned pageTime);

//@}

/**@name Dispatch controllers for specific channel types. */
//@{
void FACCHDispatcher(GSM::TCHFACCHLogicalChannel *TCHFACCH);
void SDCCHDispatcher(GSM::SDCCHLogicalChannel *SDCCH);
void DCCHDispatcher(GSM::LogicalChannel *DCCH);
//@}



/**
	Resolve a mobile ID to an IMSI.
	Returns TMSI, if it is already in the TMSITable.
	@param sameLAI True if the mobileID is known to have come from this LAI.
	@param mobID A mobile ID, that may be modified by the function.
	@param SDCCH The Dm channel to the mobile.
	@return A TMSI value from the TMSITable or zero if non found.
*/
unsigned  resolveIMSI(bool sameLAI, GSM::L3MobileIdentity& mobID, GSM::LogicalChannel* LCH);

/**
	Resolve a mobile ID to an IMSI.
	@param mobID A mobile ID, that may be modified by the function.
	@param SDCCH The Dm channel to the mobile.
*/
void  resolveIMSI(GSM::L3MobileIdentity& mobID, GSM::LogicalChannel* LCH);




/**@ Paging mechanisms */
//@{


/** An entry in the paging list. */
class PagingEntry {

	private:

	GSM::L3MobileIdentity mID;		///< The mobile ID.
	GSM::ChannelType mType;			///< The needed channel type.
	unsigned mTransactionID;		///< The associated transaction ID.
	Timeval mExpiration;			///< The expiration time for this entry.

	public:

	/**
		Create a new entry, with current timestamp.
		@param wID The ID to be paged.
		@param wLife The number of milliseconds to keep paging.
	*/
	PagingEntry(const GSM::L3MobileIdentity& wID, GSM::ChannelType wType,
			unsigned wTransactionID, unsigned wLife)
		:mID(wID),mType(wType),mTransactionID(wTransactionID),mExpiration(wLife)
	{}

	/** Access the ID. */
	const GSM::L3MobileIdentity& ID() const { return mID; }

	/** Access the channel type needed. */
	GSM::ChannelType type() const { return mType; }

	unsigned transactionID() const { return mTransactionID; }

	/** Renew the timer. */
	void renew(unsigned wLife) { mExpiration = Timeval(wLife); }

	/** Returns true if the entry is expired. */
	bool expired() const { return mExpiration.passed(); }

};

typedef std::list<PagingEntry> PagingEntryList;


/**
	The pager is a global object that generates paging messages on the CCCH.
	To page a mobile, add the mobile ID to the pager.
	The entry will be deleted automatically when it expires.
	All pager operations are linear time.
	Not much point in optimizing since the main operation is inherently linear.
*/
class Pager {

	private:

	PagingEntryList mPageIDs;				///< List of ID's to be paged.
	Mutex mLock;							///< Lock for thread-safe access.
	Signal mPageSignal;						///< signal to wake the paging loop
	Thread mPagingThread;					///< Thread for the paging loop.
	volatile bool mRunning;

	public:

	Pager()
		:mRunning(false)
	{}

	/** Set the output FIFO and start the paging loop. */
	void start();

	/**
		Add a mobile ID to the paging list.
		@param addID The mobile ID to be paged.
		@param chanType The channel type to be requested.
		@param transaction The transaction record, which will be modified.
		@param wLife The paging duration in ms, default based on SIP INVITE retry preiod, Timer A.
	*/
	void addID(
		const GSM::L3MobileIdentity& addID,
		GSM::ChannelType chanType,
		TransactionEntry& transaction,
		unsigned wLife=2*gConfig.getNum("SIP.Timer.A")
	);

	/**
		Remove a mobile ID.
		This is used to stop the paging when a phone responds.
		@return The transaction ID associated with this entry.
	*/
	unsigned removeID(const GSM::L3MobileIdentity&);

	private:

	/**
		Traverse the paging list, paging all IDs.
		@return Number of IDs paged.
	*/
	unsigned pageAll();

	/** A loop that repeatedly calls pageAll. */
	void serviceLoop();

	/** C-style adapter. */
	friend void *PagerServiceLoopAdapter(Pager*);

public:

	/** return size of PagingEntryList */
	size_t pagingEntryListSize();

	/** Dump the paging list to an ostream. */
	void dump(std::ostream&) const;
};


void *PagerServiceLoopAdapter(Pager*);


//@}	// paging mech





/**@name Transaction Table mechanisms. */
//@{

/**
	A TransactionEntry object is used to maintain the state of a transaction
	as it moves from channel to channel.
	The object itself is not thread safe.
*/
class TransactionEntry {

	public:

	/** Call states based on GSM 04.08 5 and ITU-T Q.931 */
	enum Q931CallState {
		NullState,
		Paging,
		MOCInitiated,
		MOCProceeding,
		MTCConfirmed,
		CallReceived,
		CallPresent,
		ConnectIndication,
		Active,
		DisconnectIndication,
		ReleaseRequest,
		SMSDelivering,
		SMSSubmitting,
	};

	private:

	unsigned mID;						///< the internal transaction ID, assigned by a TransactionTable

	GSM::L3MobileIdentity mSubscriber;		///< some kind of subscriber ID, preferably IMSI
	GSM::L3CMServiceType mService;			///< the associated service type
	unsigned mTIFlag;						///< "0" for originating party ,"1" for terminating
	unsigned mTIValue;						///< the L3 short transaction ID set by the MS
	GSM::L3CalledPartyBCDNumber mCalled;	///< the associated called party number, if known
	GSM::L3CallingPartyBCDNumber mCalling;	///< the associated calling party number, if known

	SIP::SIPEngine mSIP;					///< the SIP IETF RFC-3621 protocol engine
	Q931CallState mQ931State;				///< the GSM/ISDN/Q.931 call state
	Timeval mStateTimer;					///< timestamp of last state change.

	char mMessage[256];						///< text messaging payload

	/**@name Timers from GSM and Q.931 (network side) */
	//@{
	// If you add a timer, remember to add it to
	// the constructor, timerExpired and resetTimers methods.
	GSM::Z100Timer mT301;
	GSM::Z100Timer mT302;
	GSM::Z100Timer mT303;
	GSM::Z100Timer mT304;
	GSM::Z100Timer mT305;		///< a "clearing timer"
	GSM::Z100Timer mT308;		///< a "clearing timer"
	GSM::Z100Timer mT310;
	GSM::Z100Timer mT313;
	GSM::Z100Timer mT3113;		///< the paging timer, NOT a Q.931 timer
	GSM::Z100Timer mTR1M;		///< SMS RP-ACK timer, see GSM 04.11 6.2.1.2
	//@}

	public:

	TransactionEntry();

	/** This form is used for MTC or MT-SMS with TI set to 0. */
	TransactionEntry(const GSM::L3MobileIdentity& wSubscriber, 
		const GSM::L3CMServiceType& wService,
		const GSM::L3CallingPartyBCDNumber& wCalling,
		const char *wMessage = NULL);

	/** This form is used for MOC. */
	TransactionEntry(const GSM::L3MobileIdentity& wSubscriber,
		const GSM::L3CMServiceType& wService,
		unsigned wTIValue,
		const GSM::L3CalledPartyBCDNumber& wCalled);

	/** Another MT form, with controlled TI. */
	TransactionEntry(const GSM::L3MobileIdentity& wSubscriber,
		const GSM::L3CMServiceType& wService,
		unsigned wTIValue,
		const GSM::L3CallingPartyBCDNumber& wCalling);

	/**@name Accessors. */
	//@{
	unsigned TIValue() const { return mTIValue; }
	unsigned TIFlag() const { return mTIFlag; }
	void TI(unsigned wTIFlag, unsigned wTIValue)
		{ mTIFlag=wTIFlag; mTIValue = wTIValue; }

	const GSM::L3MobileIdentity& subscriber() const { return mSubscriber; }

	const GSM::L3CMServiceType& service() const { return mService; }

	const GSM::L3CalledPartyBCDNumber& called() const { return mCalled; }

	const GSM::L3CallingPartyBCDNumber& calling() const { return mCalling; }

	const char* message() const { return mMessage; }
	void message(const char *wMessage, unsigned length)
	{
		unsigned tocopy = (length > 255) ? 255 : length;
		memcpy(mMessage, wMessage, tocopy);
		mMessage[tocopy] ='\0';
	}

	unsigned ID() const { return mID; }

	SIP::SIPEngine& SIP() { return mSIP; }
	const SIP::SIPEngine& SIP() const { return mSIP; }

	void Q931State(Q931CallState wState)
	{
		mStateTimer.now();
		mQ931State=wState;
	}

	Q931CallState Q931State() const { return mQ931State; }

	unsigned stateAge() const { return mStateTimer.elapsed(); }

	/**@name Timer access. */
	// TODO -- If we were clever, this would be a table.
	//@{
	GSM::Z100Timer& T301() { return mT301; }
	GSM::Z100Timer& T302() { return mT302; }
	GSM::Z100Timer& T303() { return mT303; }
	GSM::Z100Timer& T304() { return mT304; }
	GSM::Z100Timer& T305() { return mT305; }
	GSM::Z100Timer& T308() { return mT308; }
	GSM::Z100Timer& T310() { return mT310; }
	GSM::Z100Timer& T313() { return mT313; }
	GSM::Z100Timer& T3113() { return mT3113; }
	GSM::Z100Timer& TR1M() { return mTR1M; }
	//@}
	//@}

	/** Return true if clearing is in progress. */
	bool clearing() const
		{ return (mQ931State==ReleaseRequest) || (mQ931State==DisconnectIndication); }

	/** Return true if any Q.931 timer is expired. */
	bool timerExpired() const;

	/** Reset all Q.931 timers. */
	void resetTimers();

	/** Retrns true if the transaction is "dead". */
	bool dead() const;

	private:

	friend class TransactionTable;

	void ID(unsigned wID) { mID=wID; }
};


std::ostream& operator<<(std::ostream& os, const TransactionEntry&);
std::ostream& operator<<(std::ostream& os, TransactionEntry::Q931CallState);


/** A map of transactions keyed by ID. */
class TransactionMap : public std::map<unsigned,TransactionEntry> {};

/**
	A table for tracking the states of active transactions.
	Note that transaction table add and find operations
	are pass-by-copy, not pass-by-reference.
*/
class TransactionTable {

	private:

	// FIXME -- We need to support log-time lookup by transaction ID _or_ IMSI.
	// Right now, it's log-time for transaction ID and linear time for IMSI.

	TransactionMap mTable;
	mutable Mutex mLock;
	unsigned mIDCounter;

	public:

	TransactionTable()
		// This assumes the main application uses sdevrandom.
		:mIDCounter(random())
	{ }

	/**
		Return a new ID for use in the table.
	*/
	unsigned newID();

	/**
		Insert a new entry into the table.
		@param value The entry to copy into the table.
	*/
	void add(const TransactionEntry& value);

	/**
		Update a transaction in the table.
		Uses the ID previously assigned to the TransactionEntry by add().
		@param value The new TransactionEntry to be copied in.
	*/
	void update(const TransactionEntry& value);

	/**
		Find an entry and make a copy.
		(Removes entry if it was dead.)
		@param wID The transaction ID to search.
		@param target A place to put the copy.
		@return True if successful.
	*/
	bool find(unsigned wID, TransactionEntry& target);

	/**
		Remove an entry from the table.
		@param wID The transaction ID to search.
		@return True if the ID was really in the table.
	*/
	bool remove(unsigned wID);

	/**
		Find an entry by its mobile ID.
		Also clears dead entries during search.
		@param mobileID The mobile at to search for.
		@param target A TransactionEntry to accept the found record.
		@return true is the mobile ID was foind.
	*/
	bool find(const GSM::L3MobileIdentity& mobileID, TransactionEntry& target);

	/**
		Remove "dead" entries from the table.
		A "dead" entry is a transaction that is no longer active.
	*/
	void clearDeadEntries();

	/**@Access to raw map. */
	//@{
	TransactionMap::const_iterator begin() { clearDeadEntries(); return mTable.begin(); }
	TransactionMap::const_iterator end() const { return mTable.end(); }
	//@}

	size_t size();
};

//@} // Transaction Table




/**@ TMSI mechanisms */
//@{

class TMSIRecord {

	private:

	std::string mIMSI;
	std::string mIMEI;
	Timeval mCreated;				///< Time when this TMSI was created.
	mutable Timeval mTouched;		///< Time when this TMSI was last accessed.

	public:

	TMSIRecord() {}

	TMSIRecord(const char* wIMSI):
		mIMSI(wIMSI),mIMEI("?")
	{ }
	
	TMSIRecord(const char* wIMSI, const char* wIMEI):
		mIMSI(wIMSI), mIMEI(wIMEI)
	{ }

	const char* IMSI() const { return mIMSI.c_str(); }
	const char* IMEI() const { return mIMEI.c_str(); }
	void touch() const { mTouched.now(); }

	/** Record age in seconds. */
	unsigned age() const { return mCreated.elapsed()/1000; }

	/** Time since last access in seconds. */
	unsigned touched() const { return mTouched.elapsed()/1000; }

	void save(unsigned TMSI, FILE*) const;

	/**
		Load a TMSI record from a file.
		@return TMSI or 0 on read failure.
	*/
	unsigned load(FILE*);

};

std::ostream& operator<<(std::ostream&, const TMSIRecord&);

typedef std::map<unsigned,TMSIRecord> TMSIMap;

class TMSITable {

	private:

	TMSIMap mMap;							///< IMSI/TMSI mapping
	unsigned mCounter;						///< a counter to generate new TMSIs
	unsigned mClear;						///< next TMSI to be cleared from the table
	mutable Mutex mLock;					///< concurrency control


	public:

	TMSITable()
		:mCounter(time(NULL)),
		mClear(mCounter)
	{}

	/**
		Create a new entry in the table.
		@param IMSI	The IMSI to create an entry for.
		@return The assigned TMSI.
	*/
	unsigned assign(const char* IMSI);

	/**
		Find an IMSI in the table.
		This is a log-time operation.
		@param TMSI The TMSI to find.
		@return Pointer to c-string IMSI or NULL.
	*/
	const char* IMSI(unsigned TMSI) const;


	/**
		Find an entry in the table.
		This is a log-time operation.
		@param TMSI The TMSI to find.
		@param target A TMSI record to catch the result.
		@return true if the TMSI was found.
		@return Pointer to c-string IMSI or NULL.
	*/
	bool find(unsigned TMSI, TMSIRecord& target);

	/**
		Find a TMSI in the table.
		This is a linear-time operation.
		@param IMSI The IMSI to mach.
		@return A TMSI value or zero on failure.
	*/
	unsigned TMSI(const char* IMSI) const;

	/**
		Update the record's timestamp.
	*/
	void touch(unsigned TMSI) const;

	/**
		Remove an entry from the table.
		@param TMSI The TMSI to remove.
	*/
	void erase(unsigned TMSI);

	/** Write entries as text to a stream. */
	void dump(std::ostream&) const;
	
	/** Save the table to a file. */
	void save(const char* filename) const;

	/** Load the table from a file. */
	void load(const char*filename);

	/** Clear the table completely. */
	void clear() { mMap.clear(); }

	size_t size() const;

	// FIXME -- These are not thread safe and should be removed.
	TMSIMap::const_iterator begin() const { return mMap.begin(); }
	TMSIMap::const_iterator end() const { return mMap.end(); }

	private:

	/** Erase entries, oldest first, to limit the table size. */
	void purge();

};




/**@name Control-layer exceptions. */
//@{

/**
	A control layer excpection includes a pointer to a transaction.
	The transaction might require some clean-up action, depending on the exception.
*/
class ControlLayerException {

	private:

	unsigned mTransactionID;

	public:

	ControlLayerException(unsigned wTransactionID=0)
		:mTransactionID(wTransactionID)
	{}

	unsigned transactionID() { return mTransactionID; }
};

/** Thrown when the control layer gets the wrong message */
class UnexpectedMessage : public ControlLayerException {
	public:
	UnexpectedMessage(unsigned wTransactionID=0)
		:ControlLayerException(wTransactionID)
	{}
};

/** Thrown when recvL3 returns NULL */
class ChannelReadTimeout : public ControlLayerException {
	public:
	ChannelReadTimeout(unsigned wTransactionID=0)
		:ControlLayerException(wTransactionID)
	{}
};

/** Thrown when L3 can't parse an incoming message */
class UnsupportedMessage : public ControlLayerException {
	public:
	UnsupportedMessage(unsigned wTransactionID=0)
		:ControlLayerException(wTransactionID)
	{}
};

/** Thrown when the control layer gets the wrong primitive */
class UnexpectedPrimitive : public ControlLayerException {
	public:
	UnexpectedPrimitive(unsigned wTransactionID=0)
		:ControlLayerException(wTransactionID)
	{}
};

/**  Thrown when a T3xx expires */
class Q931TimerExpired : public ControlLayerException {
	public:
	Q931TimerExpired(unsigned wTransactionID=0)
		:ControlLayerException(wTransactionID)
	{}
};


//@}


}	//Control



/**@addtogroup Globals */
//@{
/** A single global transaction table in the global namespace. */
extern Control::TransactionTable gTransactionTable;
/** A single global TMSI table in the global namespace. */
extern Control::TMSITable gTMSITable;
//@}



#endif

// vim: ts=4 sw=4
