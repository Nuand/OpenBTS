/*
* Copyright 2009 Kestrel Signal Processing, Inc.
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

#ifndef HLR_H
#define HLR_H

#include <Logger.h>
#include <Timeval.h>
#include <Threads.h>
#include <map>
#include <stdlib.h>



/** Virtual class for the HLR interface. */
class HLR {

	public:

	typedef enum {
		SUCCESS=0,		///< operation successful
		FAILURE=1,		///< operation not successful
		DELAYED=2,		///< operation successful, but effect delayed
		TRYAGAIN=3		///< operation not attempted, try again later
	} Status;

	static const Status CombinedStatus[4][4];

	virtual ~HLR() {}

	/**
		Resolve an ISDN or other numeric address to an IMSI.
		@param ISDN Any numeric address, E.164, local extension, etc.
		@return A C-string to be freed by the caller,
			 NULL if the ISDN cannot be resolved.
	*/
	virtual char* getIMSI(const char* ISDN) =0;

	/**
		Given an IMSI, return the local CLID, which should be a numeric address.
		@param IMSI The subscriber IMSI.
		@return A C-string to be freed by the caller,
			NULL if the IMSI isn't found.
	*/
	virtual char* getCLIDLocal(const char* IMSI) =0;

	/**
		Given an IMSI, return the global CLID, which should be a numeric address.
		@param IMSI The subscriber IMSI.
		@return A C-string to be freed by the caller,
			NULL if the IMSI isn't found.
	*/
	virtual char* getCLIDGlobal(const char* IMSI) =0;

	/**
		Given an IMSI, return the IP address of the most recent registration.
		@param IMSI The subscriber IMSI
		@return A C-string to be freed by the caller, "111.222.333.444:port",
			NULL if the ISMI isn't registered.
	*/
	virtual char* getRegistrationIP(const char* IMSI) =0;

	/**
		Add a new user to the HLR.
		@param IMSI The user's IMSI or SIP username.
		@param CLID The user's local CLID.
	*/
	virtual Status addUser(const char* IMSI, const char* CLID) =0;


	private:

	/**
		Add an address for a user.
		@param IMSI The existing user's address.
		@param ISDN The address to add.
	*/
	virtual Status addAddress(const char* IMSI, const char* ISDN) =0;
};



/** An entry for HLR caches: a string and a timer. */
class HLRCacheEntry {

	private:

	Timeval mTimer;
	std::string mValue;

	public:

	HLRCacheEntry() {}

	/**
		Create a cache entry.
		@param wValue THe value string.
		@param lifetime The expiration period in seconds.
	*/
	HLRCacheEntry(const char* wValue, unsigned lifetime=3600)
		:mTimer(1000*lifetime),
		mValue(wValue)
	{ }

	/** Return true if the entry is expired. */
	bool expired() const { return mTimer.passed(); }

	/** Value accessor. */
	const char* value() const { return mValue.c_str(); }
};


/** An HLR cache. */
class HLRCache {

	private:

	static const size_t MaxSize = 5000;
	mutable Mutex mLock;
	std::map<std::string, HLRCacheEntry> mTable;

	public:

	/**
		Add an item to the cahce.
		@param key The cache lookup key.
		@param value The value to cache.
		@param lifetime The time until expiration, in seconds.
	*/
	void write(const char* key, const char* value, unsigned lifetime=3600);

	/**
		Check the cache for an item, flushes expired items.
		@param key The key to check.
		@return A C-string to be freed by the caller or NULL if not found.
	*/
	char* read(const char* key);

	private:

	/** Discard expired entries. */
	void flush();

};



/** An HLR facility implemented with calls into Asterisk.  */
class AsteriskHLR : public HLR {

	private:

	Mutex mLockFDLock;
	int mLockFD;

	/**@name Caches for HLR lookups. */
	//@{
	/// A cache for ISDN->IMSI lookups.
	HLRCache mIMSICache;
	/// Caches for IMSI->CLID lookups.
	HLRCache mCLIDLocalCache;
	HLRCache mCLIDGlobalCache;
	/// A cache for registration lookup.
	HLRCache mRegistrationCache;
	//@}

	/**@ Mechamisms to limit Asterisk reloads. */
	//@{
	static const int mHoldoffTime = 2;		///< reload hold-off in SECONDS
	Timeval mLastSIPReloadTime;
	bool mNeedSIPReload;
	Timeval mLastDialplanReloadTime;
	bool mNeedDialplanReload;
	//@}
	

	public:

	AsteriskHLR():
		mNeedSIPReload(false),
		mNeedDialplanReload(false)
	{ }
	
	char* getIMSI(const char* ISDN);

	char* getCLIDLocal(const char* IMSI);
	char* getCLIDGlobal(const char* IMSI);

	char* mapCLIDGlobal(const char* IMSI);

	char* getRegistrationIP(const char* IMSI);

	Status addUser(const char* IMSI, const char* CLIDLocal);

	bool useGateway(const char *ISDN);

	private:

	Status addAddress(const char* IMSI, const char* address);

	/**
		Send a command to Asterisk and return the fist line containing the tag.
		@param command The command to send.
		@param tag The tag to match.
		@return A C-string to be freed by the caller, NULL if the key isn't found.
	*/
	char *getAsteriskLine(const char* command, const char* tag);

	/**
		Reload Asterisk sip.conf, if the timer allows.
	*/
	Status reloadSIP();

	/**
		Reload Asterisk extensions.conf, if the timer allows.
	*/
	Status reloadDialplan();

	/**
		Reload full Asterisk config.
	*/
	Status reloadConfig();

	/**
		Check the config lockfile.  Return true if lock acquired.
		Returns true if the lockfile exits.
	*/
	bool lockedConfig();
};








#endif

// vim: ts=4 sw=4
