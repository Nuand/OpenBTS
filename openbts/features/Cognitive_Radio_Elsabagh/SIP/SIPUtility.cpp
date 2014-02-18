/*
* Copyright 2008 Free Software Foundation, Inc.
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


#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

#include <signal.h>
#include <stdlib.h>

#include <ortp/ortp.h>
#include <osipparser2/osip_md5.h>
#include <osipparser2/sdp_message.h>

#include "SIPInterface.h"
#include "SIPUtility.h"


using namespace SIP;
using namespace std;



#define DEBUG 1





void SIP::get_owner_ip( osip_message_t * msg, char * o_addr ){
	
	osip_body_t * sdp_body = (osip_body_t*)osip_list_get(&msg->bodies, 0);
	char * sdp_str = sdp_body->body;

	sdp_message_t * sdp;
	sdp_message_init(&sdp);
	sdp_message_parse(sdp, sdp_str);
	strcpy(o_addr, sdp->o_addr);

}

void SIP::get_rtp_params(const osip_message_t * msg, char * port, char * ip_addr )
{
	osip_body_t * sdp_body = (osip_body_t*)osip_list_get(&msg->bodies, 0);
	char * sdp_str = sdp_body->body;

	sdp_message_t * sdp;
	sdp_message_init(&sdp);
	sdp_message_parse(sdp, sdp_str);

	strcpy(port,sdp_message_m_port_get(sdp,0));
	strcpy(ip_addr, sdp->c_connection->c_addr);
}

void SIP::make_tag(char * tag)
{
	int val = random();
	
	// just grab first 6 digits of random number
	// and map [0->24] to [a-z] 
	int k;
	for( k=0;k<5;k++){
		tag[k] = val%25+97;
		val = val >> 3;
	}
	tag[k] = '\0';
}

void SIP::make_branch( char * branch )
{
	int val = random()%100000;
	sprintf(branch,"z9hG4bK%i", val);
}

// vim: ts=4 sw=4
