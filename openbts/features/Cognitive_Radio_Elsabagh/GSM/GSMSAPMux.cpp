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



#include "GSMSAPMux.h"
#include "GSMTransfer.h"
#include "GSML1FEC.h"
#include "GSML2LAPDm.h"

#include <Logger.h>


using namespace GSM;


void SAPMux::writeHighSide(const L2Frame& frame)
{
	// The SAP may or may not be present, depending on the channel type.
	OBJLOG(DEEPDEBUG) << "SAPMux::writeHighSide " << frame;
	mLock.lock();
	mDownstream->writeHighSide(frame);
	mLock.unlock();
}



void SAPMux::writeLowSide(const L2Frame& frame)
{
	OBJLOG(DEEPDEBUG) << "SAPMux::writeLowSide SAP" << frame.SAPI() << " " << frame;
	unsigned SAPI = frame.SAPI();	
	bool data = frame.primitive()==DATA;
	if (data && (!mUpstream[SAPI])) {
		LOG(NOTICE) << "received DATA for unsupported SAP " << SAPI;
		return;
	}
	if (data) {
		mUpstream[SAPI]->writeLowSide(frame);
	} else {
		// If this is a non-data primitive, copy it out to every SAP.
		for (int i=0; i<4; i++) {
			if (mUpstream[i]) mUpstream[i]->writeLowSide(frame);
		}
	}
}



void LoopbackSAPMux::writeHighSide(const L2Frame& frame)
{
	OBJLOG(DEEPDEBUG) << "TestSAPMux::writeHighSide " << frame;
	// Substitute primitive
	L2Frame newFrame(frame);
	unsigned SAPI = frame.SAPI();	
	switch (frame.primitive()) {
		case ERROR: SAPI=0; break;
		case RELEASE: return;
		default: break;
	}
	// Because this is a test fixture, as assert here.
	// If this were not a text fixture, we would print a warning
	// and ignore the frame.
	assert(mUpstream[SAPI]);
	mLock.lock();
	mUpstream[SAPI]->writeLowSide(newFrame);
	mLock.unlock();
}



void LoopbackSAPMux::writeLowSide(const L2Frame& frame)
{
	assert(mDownstream);
	L2Frame newFrame(frame);
	mDownstream->writeHighSide(newFrame);
}





// vim: ts=4 sw=4
