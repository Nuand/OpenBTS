/*
* Copyright 2012-2013 Range Networks, Inc.
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

#include "smtest.h"

#include <string.h>
#include <stdlib.h>

#include <Sockets.h>

/** Submit an SMS for delivery.
 *  @return The server return code.
 */
int sendMessage(const char *smqueueIP, int smqueuePort, const char *myIP, int relayPort,
		const char *smscCode, const char *from, const char *to,
		const char *txtBuf, const char *contentType)
{
//	static UDPSocket sock(myPort, smqueueIP, smqueuePort);
	static UDPSocket relay(relayPort, smqueueIP, smqueuePort);

	static const char form[] =
		"MESSAGE sip:%s@%s SIP/2.0\n"
		"Via: SIP/2.0/UDP %s:%d;branch=%x\n"
		"Max-Forwards: 2\n"
		"From: %s <sip:%s@%s:%d>;tag=%d\n"
		"To: sip:%s@%s\n"
		"Call-ID: %x@%s:%d\n"
		"CSeq: 1 MESSAGE\n"
		"Content-Type: %s\n" \
		"Content-Length: %u\n"
		"\n%s\n";
	static char buffer[1500];
	snprintf(buffer, 1499, form,
		smscCode, smqueueIP,
		myIP, relayPort, (unsigned)random(),
		from, from, myIP, relayPort, (unsigned)random(),
		to, smqueueIP,
		(unsigned)random(), myIP, relayPort,
		contentType, strlen(txtBuf), txtBuf);

	printf(">>>>>>>>>>>>>>>>>>>>\n%s\n", buffer);

	relay.write(buffer);

	int numRead = relay.read(buffer,10000);
	if (numRead >= 0) {
		buffer[numRead] = '\0';

		printf("<<<<<<<<<<<<<<<<<<<<\n%s\n", buffer);
	} else {
		printf("%s\n", "Timed out");
	}

	return TEST_SUCCESS;
}

int main(int argc, const char *argv[])
{
	if (argc == 9) {
		const char *smqueueIP = argv[1]; //"127.0.0.1";
		int smqueuePort = atoi(argv[2]); //5062;
		const char *myIP = argv[3]; //"127.0.0.1";
		int relayPort = atoi(argv[4]);
		const char *smscCode = "smsc";
		const char *from = argv[5];
		const char *to = argv[6];
		const char *msg = argv[7];
		const char *contentType = argv[8];

#define doTest(x,y,z) sendMessage(smqueueIP, smqueuePort, myIP, relayPort, smscCode, x, y, z, contentType)
	
		//doTest("from", "to", "message");
		doTest(from, to, msg);
	} else {
		printf("usage: (All fields are required)\n"
			"smrelaytest smqueueIP smqueuePort myIP relayPort from to message contentType\n\n");
	}

	return 1;
}

