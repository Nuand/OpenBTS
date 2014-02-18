
#include <iostream>
#include <fstream>

#include <TRXManager.h>
#include <GSML1FEC.h>
#include <GSMConfig.h>
#include <GSMSAPMux.h>
#include <GSML3RRMessages.h>
#include <GSMLogicalChannel.h>

#include <SIPInterface.h>
#include <Globals.h>

#include <Logger.h>
#include <CLI.h>
#include <PowerManager.h>
#include <RRLPQueryController.h>
#include <Configuration.h>

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


ConfigurationTable gConfig("OpenBTS.config");



pid_t gTransceiverPid = 0;

void restartTransceiver()
{

	if (gTransceiverPid > 0) {
		LOG(INFO) << "RESTARTING TRANSCEIVER";
		kill(gTransceiverPid,SIGKILL); // TODO - call on ctrl-c (put in signal?)
	}

	const char *TRXPath = NULL;
	if (gConfig.defines("TRX.Path")) TRXPath=gConfig.getStr("TRX.Path");
	if (TRXPath) {
		const char *TRXLogLevel = gConfig.getStr("TRX.LogLevel");
		const char *TRXLogFileName = NULL;
		if (gConfig.defines("TRX.LogFileName")) TRXLogFileName=gConfig.getStr("TRX.LogFileName");
		gTransceiverPid = vfork();
		LOG_ASSERT(gTransceiverPid>=0);
		if (gTransceiverPid==0) {
			execl(TRXPath,"transceiver",TRXLogLevel,TRXLogFileName,NULL);
			LOG(ERROR) << "cannot start transceiver";
			_exit(0);
		}
	}
}




int main(int argc, char *argv[])
{
	srandom(time(NULL));

	COUT("\nStarting the system...");

	restartTransceiver();

	COUT("\n\nWelcome to Cognitve.");


return 0;
}

// vim: ts=4 sw=4
