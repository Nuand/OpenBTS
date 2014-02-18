/**
 *
 *
 */

#include "smtest.h"

#include <string.h>
#include <stdlib.h>

#include <Sockets.h>

/** Submit an SMS for delivery.
 *  @return The server return code.
 */
int sendMessage(const char *smqueueIP, int smqueuePort, const char *myIP, int myPort,
		const char *smscCode, const char *from, const char *to,
		const char *txtBuf)
{
	static UDPSocket sock(myPort, smqueueIP, smqueuePort);

	static const char form[] =
		"MESSAGE sip:%s@%s SIP/2.0\n"
		"Via: SIP/2.0/UDP %s;branch=%x\n"
		"Max-Forwards: 2\n"
		"From: %s <sip:%s@%s:%d>;tag=%d\n"
		"To: sip:%s@%s\n"
		"Call-ID: %x@%s:%d\n"
		"CSeq: 1 MESSAGE\n"
		"Content-Type: text/plain\n" \
		"Content-Length: %u\n"
		"\n%s\n";
	static char buffer[1500];
	snprintf(buffer, 1499, form,
		smscCode, smqueueIP,
		myIP, (unsigned)random(),
		from, from, myIP, myPort, (unsigned)random(),
		to, smqueueIP,
		(unsigned)random(), myIP, myPort,
		strlen(txtBuf), txtBuf);
	sock.write(buffer);

	int numRead = sock.read(buffer,10000);
	if (numRead >= 0) {
		buffer[numRead] = '\0';

		printf("%s\n", buffer);
	} else {
		printf("%s\n", "Timed out");
	}

	return TEST_SUCCESS;
}

int main(int argc, const char *argv[])
{
	if (argc == 8) {
		const char *smqueueIP = argv[1]; //"127.0.0.1";
		int smqueuePort = atoi(argv[2]); //5062;
		const char *myIP = argv[3]; //"127.0.0.1";
		int myPort = atoi(argv[4]); //5070;
		const char *smscCode = "smsc";
		const char *from = argv[5];
		const char *to = argv[6];
		const char *msg = argv[7];

#define doTest(x,y,z) sendMessage(smqueueIP, smqueuePort, myIP, myPort, smscCode, x, y, z)
	
		//doTest("from", "to", "message");
		doTest(from, to, msg);
	} else {
		printf("usage: (All fields are required)\n"
			"smtest smqueueIP smqueuePort myIP myPort from to message\n\n");
	}

	return 1;
}

