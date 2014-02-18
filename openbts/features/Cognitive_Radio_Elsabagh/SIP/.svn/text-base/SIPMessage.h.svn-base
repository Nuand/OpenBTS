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




#ifndef SIP_MESSAGE_H
#define SIP_MESSAGE_H

namespace SIP {



osip_message_t * sip_register( const char * sip_username, short timeout, short local_port, const char * local_ip, 
const char * proxy_ip, const char * from_tag, const char * via_branch, const char * call_id, int cseq);


osip_message_t * sip_unregister( const char * sip_username, short local_port, const char * local_ip, 
const char * proxy_ip, const char * from_tag, const char * via_branch, const char * call_id, int cseq);


osip_message_t * sip_message( const char * dialed_number, const char * sip_username, short local_port, const char * local_ip, const char * proxy_ip, const char * from_tag, const char * via_branch, const char * call_id, int cseq, const char* message);

osip_message_t * sip_invite( const char * dialed_number, short rtp_port,const char * sip_username, short local_port, const char * local_ip, const char * proxy_ip, const char * from_tag, const char * via_branch, const char * call_id, int cseq, unsigned codec);


osip_message_t * sip_ack( const char * req_uri, const char * dialed_number, const char * sip_username, short wlocal_port, const char * local_ip, const char * proxy_ip, const char * from_tag, const char * to_tag, const char * via_branch, const char * call_id, int cseq);


osip_message_t * sip_bye( const char * req_uri, const char * dialed_number, const char * sip_username, short local_port, const char * local_ip, const char * proxy_ip, const char * from_tag, const char * to_tag, const char * via_branch, const char * call_id, int cseq);


osip_message_t * sip_okay( osip_message_t * inv, const char * sip_username, const char * local_ip, short wlocal_port, const char * to_tag, short rtp_port, unsigned audio_codecs );

osip_message_t * sip_okay_SMS( osip_message_t * inv, const char * sip_username, const char * local_ip, short wlocal_port, const char * to_tag);

osip_message_t * sip_info(unsigned info, const char *dialed_number, short rtp_port,const char * sip_username, short local_port, const char * local_ip, const char * proxy_ip, const char * from_tag, const char * via_branch, const char * call_id, int cseq);

osip_message_t * sip_b_okay( osip_message_t * bye  );

osip_message_t * sip_trying( osip_message_t * invite, const char * sip_username, const char * local_ip);

osip_message_t * sip_ringing( osip_message_t * invite, const char * sip_username, const char * local_ip, 
const char * to_tag );



};
#endif

