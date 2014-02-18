/*
* Copyright 2008, 2009, 2010 Free Software Foundation, Inc.
* Copyright 2010 Kestrel Signal Processing, Inc.
* Copyright 2011 Range Networks, Inc.
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

#include <iostream>
#include <fstream>

#include <Configuration.h>
// Load configuration from a file.
ConfigurationTable gConfig("/etc/OpenBTS/OpenBTS.db");

#include <TRXManager.h>
#include <GSML1FEC.h>
#include <GSMConfig.h>
#include <GSMSAPMux.h>
#include <GSML3RRMessages.h>
#include <GSMLogicalChannel.h>

#include <ControlCommon.h>
#include <TransactionTable.h>

#include <SIPInterface.h>
#include <Globals.h>

#include <Logger.h>
#include <CLI.h>
#include <PowerManager.h>
#include <Configuration.h>
#include <PhysicalStatus.h>
#include <SubscriberRegistry.h>

#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#ifdef HAVE_LIBREADLINE // [
//#  include <stdio.h>
#  include <readline/readline.h>
#  include <readline/history.h>
#endif // HAVE_LIBREADLINE ]

using namespace std;
using namespace GSM;


const char* gDateTime = __DATE__ " " __TIME__;


// All of the other globals that rely on the global configuration file need to
// be declared here.

// The TMSI Table.
Control::TMSITable gTMSITable(gConfig.getStr("Control.Reporting.TMSITable").c_str());

// The transaction table.
Control::TransactionTable gTransactionTable;

// Physical status reporting
GSM::PhysicalStatus gPhysStatus(gConfig.getStr("Control.Reporting.PhysStatusTable").c_str());

// The global SIPInterface object.
SIP::SIPInterface gSIPInterface;

// Configure the BTS object based on the config file.
// So don't create this until AFTER loading the config file.
GSMConfig gBTS;

// Our interface to the software-defined radio.
TransceiverManager gTRX(gConfig.getStr("TRX.IP").c_str(), gConfig.getNum("TRX.Port"));

// Subscriber registry
SubscriberRegistry gSubscriberRegistry;


/** Define a function to call any time the configuration database changes. */
void purgeConfig(void*,int,char const*, char const*, sqlite3_int64)
{
	LOG(INFO) << "purging configuration cache";
	gConfig.purge();
	gBTS.regenerateBeacon();
}



const char* transceiverPath = "./transceiver";

pid_t gTransceiverPid = 0;

void startTransceiver()
{
	// Start the transceiver binary, if the path is defined.
	// If the path is not defined, the transceiver must be started by some other process.
	char TRXnumARFCN[4];
	sprintf(TRXnumARFCN,"%1d",1);
	LOG(NOTICE) << "starting transceiver " << transceiverPath << " " << TRXnumARFCN;
	gTransceiverPid = vfork();
	LOG_ASSERT(gTransceiverPid>=0);
	if (gTransceiverPid==0) {
		// Pid==0 means this is the process that starts the transceiver.
		execlp(transceiverPath,transceiverPath,TRXnumARFCN,NULL);
		LOG(EMERG) << "cannot find " << transceiverPath;
		_exit(1);
	}
}




int main(int argc, char *argv[])
{

	try {

	srandom(time(NULL));

	gConfig.setUpdateHook(purgeConfig);
	gLogInit("openbts",gConfig.getStr("Log.Level").c_str(),LOG_LOCAL7);
	LOG(ALERT) << "OpenBTS starting, ver " << VERSION << " build date " << __DATE__;

	COUT("\n\n" << gOpenBTSWelcome << "\n");
	COUT("\nStarting the system...");

	startTransceiver();

	// Start the SIP interface.
	gSIPInterface.start();


	//
	// Configure the radio.
	//

	// Start the transceiver interface.
	// Sleep long enough for the USRP to bootload.
	sleep(5);
	gTRX.start();

	// Set up the interface to the radio.
	// Get a handle to the C0 transceiver interface.
	ARFCNManager* C0radio = gTRX.ARFCN();

	// Tuning.
	// Make sure its off for tuning.
	C0radio->powerOff();
	// Get the ARFCN list.
	unsigned C0 = gConfig.getNum("GSM.Radio.C0");
	// Tune the radio.
	LOG(INFO) << "tuning TRX to ARFCN " << C0;
	ARFCNManager* radio = gTRX.ARFCN();
	radio->tune(C0);

	// Set TSC same as BCC everywhere.
	C0radio->setTSC(gBTS.BCC());

	// Set maximum expected delay spread.
	C0radio->setMaxDelay(gConfig.getNum("GSM.Radio.MaxExpectedDelaySpread"));

	// Set Receiver Gain
	C0radio->setRxGain(gConfig.getNum("GSM.Radio.RxGain"));

	// Turn on and power up.
	C0radio->powerOn();
	C0radio->setPower(gConfig.getNum("GSM.Radio.PowerManager.MinAttenDB"));

	//
	// Create a C-V channel set on C0T0.
	//

	// C-V on C0T0
	C0radio->setSlot(0,5);
	// SCH
	SCHL1FEC SCH;
	SCH.downstream(C0radio);
	SCH.open();
	// FCCH
	FCCHL1FEC FCCH;
	FCCH.downstream(C0radio);
	FCCH.open();
	// BCCH
	BCCHL1FEC BCCH;
	BCCH.downstream(C0radio);
	BCCH.open();
	// RACH
	RACHL1FEC RACH(gRACHC5Mapping);
	RACH.downstream(C0radio);
	RACH.open();
	// CCCHs
	CCCHLogicalChannel CCCH0(gCCCH_0Mapping);
	CCCH0.downstream(C0radio);
	CCCH0.open();
	CCCHLogicalChannel CCCH1(gCCCH_1Mapping);
	CCCH1.downstream(C0radio);
	CCCH1.open();
	CCCHLogicalChannel CCCH2(gCCCH_2Mapping);
	CCCH2.downstream(C0radio);
	CCCH2.open();
	// use CCCHs as AGCHs
	gBTS.addAGCH(&CCCH0);
	gBTS.addAGCH(&CCCH1);
	gBTS.addAGCH(&CCCH2);

	// C-V C0T0 SDCCHs
	SDCCHLogicalChannel C0T0SDCCH[4] = {
		SDCCHLogicalChannel(0,gSDCCH_4_0),
		SDCCHLogicalChannel(0,gSDCCH_4_1),
		SDCCHLogicalChannel(0,gSDCCH_4_2),
		SDCCHLogicalChannel(0,gSDCCH_4_3),
	};
	Thread C0T0SDCCHControlThread[4];
	for (int i=0; i<4; i++) {
		C0T0SDCCH[i].downstream(C0radio);
		C0T0SDCCHControlThread[i].start((void*(*)(void*))Control::DCCHDispatcher,&C0T0SDCCH[i]);
		C0T0SDCCH[i].open();
		gBTS.addSDCCH(&C0T0SDCCH[i]);
	}


	//
	// Configure the other slots.
	//

	// Count configured slots.
	unsigned sCount = 1;

	if (gConfig.defines("GSM.Channels.C1sFirst")) {
		// Create C-I slots.
		for (int i=0; i<gConfig.getNum("GSM.Channels.NumC1s"); i++) {
			gBTS.createCombinationI(gTRX,sCount);
			sCount++;
		}
	}

	// Create C-VII slots.
	for (int i=0; i<gConfig.getNum("GSM.Channels.NumC7s"); i++) {
		gBTS.createCombinationVII(gTRX,sCount);
		sCount++;
	}

	if (!gConfig.defines("GSM.Channels.C1sFirst")) {
		// Create C-I slots.
		for (int i=0; i<gConfig.getNum("GSM.Channels.NumC1s"); i++) {
			gBTS.createCombinationI(gTRX,sCount);
			sCount++;
		}
	}


	// Set up idle filling on C0 as needed.
	while (sCount<8) {
		gBTS.createCombination0(gTRX,sCount);
		sCount++;
	}

	/*
		Note: The number of different paging subchannels on       
		the CCCH is:                                        
                                                           
		MAX(1,(3 - BS-AG-BLKS-RES)) * BS-PA-MFRMS           
			if CCCH-CONF = "001"                        
		(9 - BS-AG-BLKS-RES) * BS-PA-MFRMS                  
			for other values of CCCH-CONF               
	*/

	// Set up the pager.
	// Set up paging channels.
	// HACK -- For now, use a single paging channel, since paging groups are broken.
	gBTS.addPCH(&CCCH2);

	// Be sure we are not over-reserving.
	LOG_ASSERT(gConfig.getNum("GSM.CCCH.PCH.Reserve")<(int)gBTS.numAGCHs());

	// OK, now it is safe to start the BTS.
	gBTS.start();

#ifdef HAVE_LIBREADLINE // [
	// start console
	using_history();

	static const char * const history_file_name = "/.openbts_history";
	char *history_name = 0;
	char *home_dir = getenv("HOME");

	if(home_dir) {
		size_t home_dir_len = strlen(home_dir);
		size_t history_file_len = strlen(history_file_name);
		size_t history_len = home_dir_len + history_file_len + 1;
		if(history_len > home_dir_len) {
			if(!(history_name = (char *)malloc(history_len))) {
				LOG(ERR) << "malloc failed: " << strerror(errno);
				exit(2);
			}
			memcpy(history_name, home_dir, home_dir_len);
			memcpy(history_name + home_dir_len, history_file_name,
			   history_file_len + 1);
			read_history(history_name);
		}
	}
#endif // HAVE_LIBREADLINE ]



	LOG(INFO) << "system ready";
	COUT("\n\nWelcome to OpenBTS.  Type \"help\" to see available commands.");
        // FIXME: We want to catch control-d (emacs keybinding for exit())


	// The logging parts were removed from this loop.
	// If we want them back, they will need to go into their own thread.
	while (1) {
#ifdef HAVE_LIBREADLINE // [
		char *inbuf = readline(gConfig.getStr("CLI.Prompt").c_str());
		if (!inbuf) break;
		if (*inbuf) {
			add_history(inbuf);
			// The parser returns -1 on exit.
			if (gParser.process(inbuf, cout, cin)<0) {
				free(inbuf);
				break;
			}
		}
		free(inbuf);
#else // HAVE_LIBREADLINE ][
		cout << endl << gConfig.getStr("CLI.Prompt");
		cout.flush();
		char inbuf[1024];
		cin.getline(inbuf,1024,'\n');
		// The parser returns -1 on exit.
		if (gParser.process(inbuf,cout,cin)<0) break;
#endif // !HAVE_LIBREADLINE ]
	}

#ifdef HAVE_LIBREADLINE // [
	if(history_name) {
		int e = write_history(history_name);
		if(e) {
			fprintf(stderr, "error: history: %s\n", strerror(e));
		}
		free(history_name);
		history_name = 0;
	}
#endif // HAVE_LIBREADLINE ]

	if (gTransceiverPid) kill(gTransceiverPid, SIGKILL);


	}

	catch (ConfigurationTableKeyNotFound e) {

		LOG(ALERT) << "configuration key " << e.key() << " not defined";
		exit(2);
	}

}

// vim: ts=4 sw=4
