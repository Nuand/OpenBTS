/*
* Copyright 2009, 2010 Kestrel Signal Processing, Inc.
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

#include "HLR.h"
#include <iostream>
#include <Logger.h>
#include <Configuration.h>

ConfigurationTable gConfig;

int main(int argc, char *argv[]) {

	AsteriskHLR gHLR;

	gLogInit("DEBUG");

	if (argc!=2) {
		std::cerr << "usage: " << argv[0] << " <number>" << std::endl;
		exit(-1);
	}
	const char *targ = argv[1];

	char *IMSI = gHLR.getIMSI(targ);
	if (IMSI) std::cout << "IMSI for " << targ << " is " << IMSI << std::endl;
	else std::cout << "no IMSI found for " << targ << std::endl;

	char *CLID = gHLR.getCLIDLocal(IMSI);
	if (CLID) std::cout << "CLID for " << IMSI << " is " << CLID << std::endl;
	else std::cout << "no CLID found for " << IMSI << std::endl;

	char *regIP = gHLR.getRegistrationIP("234100223456161");
	if (regIP) std::cout << "registration IP for " << IMSI << " is " << regIP << std::endl;
	else std::cout << "no regIP found for " << IMSI << std::endl;

	IMSI = gHLR.getIMSI(targ);
	if (IMSI) std::cout << "IMSI for " << targ << " is " << IMSI << std::endl;
	else std::cout << "no IMSI found for " << targ << std::endl;

	CLID = gHLR.getCLIDLocal(IMSI);
	if (CLID) std::cout << "CLID for " << IMSI << " is " << CLID << std::endl;
	else std::cout << "no CLID found for " << IMSI << std::endl;


	const char targ2[] = "1234567890";
	gHLR.addUser("123456789012345",targ2);

	sleep(2);

	IMSI = gHLR.getIMSI(targ2);
	if (IMSI) std::cout << "IMSI for " << targ2 << " is " << IMSI << std::endl;
	else std::cout << "no IMSI found for " << targ2 << std::endl;

	CLID = gHLR.getCLIDLocal(IMSI);
	if (CLID) std::cout << "CLID for " << IMSI << " is " << CLID << std::endl;
	else std::cout << "no CLID found for " << IMSI << std::endl;



}

