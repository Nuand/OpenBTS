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


#include "GSMConfig.h"
#include "GSMTransfer.h"
#include "GSMLogicalChannel.h"
#include <Logger.h>



using namespace std;
using namespace GSM;



GSMConfig::GSMConfig()
	:mBand((GSMBand)gConfig.getNum("GSM.Band")),
	mSI5Frame(UNIT_DATA),mSI6Frame(UNIT_DATA),
	mT3122(gConfig.getNum("GSM.T3122Min")),
	mStartTime(::time(NULL))
{
	regenerateBeacon();
}

void GSMConfig::start()
{
	mPowerManager.start();
	// Do not call this until the paging channels are installed.
	mPager.start();
}




void GSMConfig::regenerateBeacon()
{
	// Update everything from the configuration.
	LOG(NOTICE) << "regenerating system information messages";

	// BSIC components
	mNCC = gConfig.getNum("GSM.NCC");
	LOG_ASSERT(mNCC<8);
	mBCC = gConfig.getNum("GSM.BCC");
	LOG_ASSERT(mBCC<8);

	// MCC/MNC/LAC
	mLAI = L3LocationAreaIdentity();

	// Now regenerate all of the system information messages.

	// an L3 frame to use
	L3Frame l3(UNIT_DATA);

	// SI1
	L3SystemInformationType1 SI1;
	LOG(INFO) << SI1;
	SI1.write(l3);
	L2Header SI1Header(L2Length(l3.length()));
	mSI1Frame = L2Frame(SI1Header,l3);
	LOG(DEBUG) << "mSI1Frame " << mSI1Frame;

	// SI2
	L3SystemInformationType2 SI2;
	LOG(INFO) << SI2;
	SI2.write(l3);
	L2Header SI2Header(L2Length(l3.length()));
	mSI2Frame = L2Frame(SI2Header,l3);
	LOG(DEBUG) << "mSI2Frame " << mSI2Frame;

	// SI3
	L3SystemInformationType3 SI3;
	LOG(INFO) << SI3;
	SI3.write(l3);
	L2Header SI3Header(L2Length(l3.length()));
	mSI3Frame = L2Frame(SI3Header,l3);
	LOG(DEBUG) << "mSI3Frame " << mSI3Frame;

	// SI4
	L3SystemInformationType4 SI4;
	LOG(INFO) << SI4;
	SI4.write(l3);
	L2Header SI4Header(L2Length(l3.length()));
	mSI4Frame = L2Frame(SI4Header,l3);
	LOG(DEBUG) << "mSI4Frame " << mSI4Frame;

	// SI5
	L3SystemInformationType5 SI5;
	LOG(INFO) << SI5;
	SI5.write(mSI5Frame);
	LOG(DEBUG) << "mSI5Frame " << mSI5Frame;

	// SI6
	L3SystemInformationType6 SI6;
	LOG(INFO) << SI6;
	SI6.write(mSI6Frame);
	LOG(DEBUG) "mSI6Frame " << mSI6Frame;

}





CCCHLogicalChannel* GSMConfig::minimumLoad(CCCHList &chanList)
{
	if (chanList.size()==0) return NULL;
	CCCHList::iterator chan = chanList.begin();
	CCCHLogicalChannel *retVal = *chan;
	unsigned minLoad = (*chan)->load();
	++chan;
	while (chan!=chanList.end()) {
		unsigned thisLoad = (*chan)->load();
		if (thisLoad<minLoad) {
			minLoad = thisLoad;
			retVal = *chan;
		}
		++chan;
	}
	return retVal;
}






template <class ChanType> ChanType* getChan(vector<ChanType*>& chanList)
{
	const unsigned sz = chanList.size();
	if (sz==0) return NULL;
	// Start the search from a random point in the list.
	//unsigned pos = random() % sz;
	// HACK -- Try in-order allocation for debugging.
	for (unsigned i=0; i<sz; i++) {
		ChanType *chan = chanList[i];
		//ChanType *chan = chanList[pos];
		if (chan->recyclable()) return chan;
		//pos = (pos+1) % sz;
	}
	return NULL;
}





SDCCHLogicalChannel *GSMConfig::getSDCCH()
{
	mLock.lock();
	SDCCHLogicalChannel *chan = getChan<SDCCHLogicalChannel>(mSDCCHPool);
	if (chan) chan->open();
	mLock.unlock();
	return chan;
}


TCHFACCHLogicalChannel *GSMConfig::getTCH()
{
	mLock.lock();
	TCHFACCHLogicalChannel *chan = getChan<TCHFACCHLogicalChannel>(mTCHPool);
	if (chan) chan->open();
	mLock.unlock();
	return chan;
}



template <class ChanType> size_t chanAvailable(const vector<ChanType*>& chanList)
{
	size_t count = 0;
	for (unsigned i=0; i<chanList.size(); i++) {
		if (chanList[i]->recyclable()) count++;
	}
	return count;
}



size_t GSMConfig::SDCCHAvailable() const
{
	mLock.lock();
	size_t retVal = chanAvailable<SDCCHLogicalChannel>(mSDCCHPool);
	mLock.unlock();
	return retVal;
}

size_t GSMConfig::TCHAvailable() const
{
	mLock.lock();
	size_t retVal = chanAvailable<TCHFACCHLogicalChannel>(mTCHPool);
	mLock.unlock();
	return retVal;
}


size_t GSMConfig::totalLoad(const CCCHList& chanList) const
{
	size_t total = 0;
	for (int i=0; i<chanList.size(); i++) {
		total += chanList[i]->load();
	}
	return total;
}



template <class ChanType> unsigned countActive(const vector<ChanType*>& chanList)
{
	unsigned active = 0;
	const unsigned sz = chanList.size();
	// Start the search from a random point in the list.
	for (unsigned i=0; i<sz; i++) {
		if (!chanList[i]->recyclable()) active++;
	}
	return active;
}


unsigned GSMConfig::SDCCHActive() const
{
	return countActive(mSDCCHPool);
}

unsigned GSMConfig::TCHActive() const
{
	return countActive(mTCHPool);
}


unsigned GSMConfig::T3122() const
{
	mLock.lock();
	unsigned retVal = mT3122;
	mLock.unlock();
	return retVal;
}

unsigned GSMConfig::growT3122()
{
	unsigned max = gConfig.getNum("GSM.T3122Max");
	mLock.lock();
	unsigned retVal = mT3122;
	mT3122 += (random() % mT3122) / 2;
	if (mT3122>max) mT3122=max;
	mLock.unlock();
	return retVal;
}


unsigned GSMConfig::shrinkT3122()
{
	unsigned min = gConfig.getNum("GSM.T3122Min");
	mLock.lock();
	unsigned retVal = mT3122;
	mT3122 -= (random() % mT3122) / 2;
	if (mT3122<min) mT3122=min;
	mLock.unlock();
	return retVal;
}



void GSMConfig::createCombination0(TransceiverManager& TRX, unsigned CN, unsigned TN)
{
	// This channel is a dummy burst generator.
	// This should not be applied to non-C0.
	LOG_ASSERT(CN==0);
	// This should not be applied to C0T0.
	LOG_ASSERT(TN!=0);
	LOG(NOTICE) << "Configuring dummy filling on C" << CN << "T " << TN;
	ARFCNManager *radio = TRX.ARFCN(CN);
	radio->setSlot(TN,0);
}


void GSMConfig::createCombinationI(TransceiverManager& TRX, unsigned CN, unsigned TN)
{
	LOG_ASSERT((CN!=0)||(TN!=0));
	LOG(NOTICE) << "Configuring combination I on C" << CN << "T" << TN;
	ARFCNManager *radio = TRX.ARFCN(CN);
	radio->setSlot(TN,1);
	TCHFACCHLogicalChannel* chan = new TCHFACCHLogicalChannel(TN,gTCHF_T[TN]);
	chan->downstream(radio);
	Thread* thread = new Thread;
	thread->start((void*(*)(void*))Control::DCCHDispatcher,chan);
	chan->open();
	gBTS.addTCH(chan);

}


void GSMConfig::createCombinationVII(TransceiverManager& TRX, unsigned CN, unsigned TN)
{
	LOG_ASSERT((CN!=0)||(TN!=0));
	LOG(NOTICE) << "Configuring combination VII on C" << CN << "T" << TN;
	ARFCNManager *radio = TRX.ARFCN(CN);
	radio->setSlot(TN,7);
	for (int i=0; i<8; i++) {
		SDCCHLogicalChannel* chan = new SDCCHLogicalChannel(TN,gSDCCH8[i]);
		chan->downstream(radio);
		Thread* thread = new Thread;
		thread->start((void*(*)(void*))Control::DCCHDispatcher,chan);
		chan->open();
		gBTS.addSDCCH(chan);
	}
}


void GSMConfig::hold(bool val)
{
	mLock.lock();
	mHold = val;
	mLock.unlock();
}

bool GSMConfig::hold() const
{
	mLock.lock();
	bool val = mHold;
	mLock.unlock();
	return val;
}



// vim: ts=4 sw=4
