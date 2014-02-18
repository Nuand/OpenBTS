/*
 * Smqueue.cpp - In-memory queue manager for Short Messages (SMS's) for OpenBTS.
 * Written by John Gilmore, July 2009.
 *
 * Copyright 2009 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * See the COPYING file in the main directory for details.
 */

#include "smqueue.h"
#include "smnet.h"
#include <time.h>
#include <osipparser2/osip_message.h>	/* from osipparser2 */
#include <iostream>
#include <fstream>
#include <cstdlib>			// strtol
#include <errno.h>
#include <sys/types.h>          // open
#include <sys/stat.h>           // open
#include <fcntl.h>          // open
#include <ctype.h>		// isdigit

#include <Logger.h>
#include <Configuration.h>	// DAB

using namespace std;
using namespace SMqueue;

/* The global config table. */
// DAB
ConfigurationTable gConfig("smqueue.config");


/* We try to centralize most of the timeout values into this table.
   Occasionally the code might do something different where it knows
   better, but most state transitions just use this table to set the
   timeout that will fire if nothing happens in the new state for X
   seconds. */

/* Timeouts when going from NO_STATE into each subsequent state */
#define NT	6000	/* seconds = 50 minutes - "No Timeout" - for states
			   where we will only time out if something is really
			   broken. */
#define RT	600	/* seconds = 5 minutes - "Re Try" - for state
			   transitions where we're starting over from
			   scratch due to some error. */
/* Timeout when moving from this state to new state:
 NS  RF  AF   WD  RD  AD   WS  RS  AS   WM  RM  AM   DM   WR  RH  AR  */
int timeouts_NO_STATE[STATE_MAX_PLUS_ONE] = {
 NT,  0, NT,  NT,  0, NT,  NT,  0, NT,  NT,  0, NT,   0,  NT, NT, NT,};
int timeouts_REQUEST_FROM_ADDRESS_LOOKUP[STATE_MAX_PLUS_ONE] = {
  0, 10, 10,  NT,  0, NT,  NT, NT, NT,  NT, NT, NT,   0,   1,  0, NT,};
int timeouts_ASKED_FOR_FROM_ADDRESS_LOOKUP[STATE_MAX_PLUS_ONE] = {
  0, 60, NT,  NT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_AWAITING_TRY_DESTINATION_IMSI[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_REQUEST_DESTINATION_IMSI[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT,  0, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_ASKED_FOR_DESTINATION_IMSI[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_AWAITING_TRY_DESTINATION_SIPURL[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_REQUEST_DESTINATION_SIPURL[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  NT,  0, NT,   0,  NT, NT, NT,};
int timeouts_ASKED_FOR_DESTINATION_SIPURL[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,  NT, NT, NT,};
int timeouts_AWAITING_TRY_MSG_DELIVERY[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, NT, NT,  75,  0, NT,   0,  NT, NT, NT,};
int timeouts_REQUEST_MSG_DELIVERY[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  RT, NT, NT,  NT, 75, NT,  75, 75, 15,   0,  NT, NT, NT,};
int timeouts_ASKED_FOR_MSG_DELIVERY[STATE_MAX_PLUS_ONE] = {
  0, RT, NT,  NT, NT, NT,  NT, NT, NT,  60, 10, NT,   0,  NT, NT, NT,};
int timeouts_DELETE_ME_STATE[STATE_MAX_PLUS_ONE] = {
  0,  0,  0,   0,  0,  0,   0,  0,  0,   0,  0,  0,   0,   0,  0,  0,};
int timeouts_AWAITING_REGISTER_HANDSET[STATE_MAX_PLUS_ONE] = {
  0,  0, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,   1,  0, NT,};
int timeouts_REGISTER_HANDSET[STATE_MAX_PLUS_ONE] = {
  0,  0, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,   1,  1,  2,};
int timeouts_ASKED_TO_REGISTER_HANDSET[STATE_MAX_PLUS_ONE] = {
  0,  0, NT,  RT, NT, NT,  NT, NT, NT,  NT, NT, NT,   0,   1,  1, 10,};
/* Timeout when moving from this state to new state:
 NS  RF  AF   WD  RD  AD   WS  RS  AS   WM  RM  AM   DM   WR  RH  AR  */

#undef NT	/* No longer needed */
#undef RT

/* Index to all timeouts.  Keep in order!  */
int (*SMqueue::timeouts[STATE_MAX_PLUS_ONE])[STATE_MAX_PLUS_ONE] = {
	&timeouts_NO_STATE,
	&timeouts_REQUEST_FROM_ADDRESS_LOOKUP,
	&timeouts_ASKED_FOR_FROM_ADDRESS_LOOKUP,

	&timeouts_AWAITING_TRY_DESTINATION_IMSI,
	&timeouts_REQUEST_DESTINATION_IMSI,
	&timeouts_ASKED_FOR_DESTINATION_IMSI,

	&timeouts_AWAITING_TRY_DESTINATION_SIPURL,
	&timeouts_REQUEST_DESTINATION_SIPURL,
	&timeouts_ASKED_FOR_DESTINATION_SIPURL,

	&timeouts_AWAITING_TRY_MSG_DELIVERY,
	&timeouts_REQUEST_MSG_DELIVERY,
	&timeouts_ASKED_FOR_MSG_DELIVERY,

	&timeouts_DELETE_ME_STATE,

	&timeouts_AWAITING_REGISTER_HANDSET,
	&timeouts_REGISTER_HANDSET,
	&timeouts_ASKED_TO_REGISTER_HANDSET,
};

string SMqueue::sm_state_strings[STATE_MAX_PLUS_ONE] = {
	"No State",
	"Request From-Address Lookup",
	"Asked for From-Address",
	"Awaiting Try Destination IMSI",
	"Request Destination IMSI",
	"Asked for Destination IMSI",
	"Awaiting Try Destination SIP URL",
	"Request Destination SIP URL",
	"Asked for Destination SIP URL",
	"Awaiting Try Message Delivery",
	"Request Message Delivery",
	"Asked for Message Delivery",
	"Delete Me",
	"Awaiting Register Handset",
	"Register Handset",
	"Asked to Register Handset",
};

string sm_state_string(enum sm_state astate)
{
	if (astate < STATE_MAX_PLUS_ONE)
		return sm_state_strings[astate];
	else	return "Invalid State Number";
}

/* Global variables */
bool SMqueue::osip_initialized = false;	// Have we called lib's initializer?
struct osip *SMqueue::osipptr = NULL;	// Ptr to struct sorta used by library
const char *short_msg_pending::smp_my_ipaddress = NULL;	// Accessible copy
const char *short_msg_pending::smp_my_2nd_ipaddress = NULL;	// Accessible copy

bool print_as_we_validate = false;      // Debugging

/*
 * Associative map between target phone numbers (short codes) and
 * function pointers that implement those numbers.
 */
short_code_map_t short_code_map;

/* Release memory from osip library */
void
osip_mem_release()
{
	if (osip_initialized) {
		osip_release (SMqueue::osipptr);
		SMqueue::osipptr = NULL;
		SMqueue::osip_initialized = false;
	}
}


/* ==== FIXME KLUDGE ====
 * Table of IMSIs and phone numbers, for translation.
 * This is only for test-bench use.  Real life uses the Home Location
 * Register (../HLR), currently implemented via Asterisk.
 */
static
struct imsi_phone { char imsi[4+15+1]; char phone[1+15+1]; } imsi_phone[] = {
	{"IMSI666410186585295", "+17074700741"}, 	/* Nokia 8890 */
	{"IMSI777100223456161", "+17074700746"},	/* Palm Treo */
	{{0}, {0}}
};



/* Functions */
enum sm_state
handle_sms_message(short_msg_pending *qmsg);

bool
check_to_user(char *user);

char *
read_short_msg_from_file(char *file);

/*
 * Utility functions
 */

void
SMqueue::abfuckingort() { (*(char *)0) = 127; }   // where is the real one??

// C++ wants you to use <string>.  Fuck that.  When we want dynamically
// allocated char *'s, then we'll use them.  strdup uses malloc, tho, so
// we need a "new/delete" oriented strdup.  Ok, simple.
char *
SMqueue::new_strdup(const char *orig)
{
	int len = strlen(orig);
	char *result = new char[len+1];
	strncpy(result, orig, len+1);
	return result;
}


/* 
 * Main state machine for Short Message processing.
 */

/* Timeouts.  Back up and try again, sometimes after waiting a while.  */

void
SMq::process_timeout()
{
	time_t now = time(NULL);
	short_msg_p_list::iterator qmsg;
	enum sm_state newstate;

	/* When we modify a timestamp below (in the set_state function),
	   we re-queue the message within the queue, so we have to
	   restart the iterator every time around the loop.   In effect,
	   we're always looking at the top thing on the list (thus the
	   earliest one in time).  */
	
	while (true) {
		qmsg = time_sorted_list.begin();
		if (qmsg == time_sorted_list.end())
			return;			/* Empty queue */
		if (qmsg->next_action_time > now)
			return;			/* Wait til later to do more */

		switch (qmsg->state) {
		case NO_STATE:
			// Messages in NO_STATE have errors in them.
			// Dump it to the log, and delete it, so the queue
			// won't build up.
			qmsg->make_text_valid();
			LOG(NOTICE) << "== This message had an error and is being deleted:"
			     << endl << "MSG = " << qmsg->text;
			// Fall thru into DELETE_ME_STATE!
		case DELETE_ME_STATE:
			// This message should quietly go away.
	            {
			short_msg_p_list temp;
			// Extract the current sm from the time_sorted_list
			temp.splice(temp.begin(), time_sorted_list, qmsg);
			// When we remove it from the new "temp" list,
			// this entry will be deallocated.  qmsg still
			// points to its (dead) storage, so be careful
			// not to reference qmsg (I don't know a C++ way
			// to set it to NULL or delete it or something).
			temp.pop_front();
		    }
			break;

		default: 
			LOG(NOTICE) << "Message timed out with bad state "
			     << qmsg->state << " and message: " << qmsg->text;
			set_state(qmsg, REQUEST_FROM_ADDRESS_LOOKUP);
		/* NO BREAK */
		case REQUEST_FROM_ADDRESS_LOOKUP:
			// This is the initial state in which a message
			// enters the system.  Here, the message could be
			// a SIP response as well as a SIP MESSAGE -- or
			// something we can't process like a SIP REGISTER
			// or garbage.  Well, actually, garbage was rejected
		        // earlier before queue insertion.

			/* From this point onward, we're going to assume
			   that the message has valid, reasonable headers
			   and contents for our purposes.  Centralize all
			   that checking in validate_short_msg(). */

			if (MSG_IS_REQUEST(qmsg->parsed)) {
				// It's a MESSAGE or invalid REGISTER
				// Deal with it!
				newstate = handle_sms_message(qmsg);
				set_state(qmsg, newstate);
				break;
			} else {
				// It's a RESPONSE.
				handle_response(qmsg);
				// Probably the RESPONSE has been deleted.
				// We go back to top of loop.
				break;
			}
			break;

		case REQUEST_DESTINATION_IMSI:
			/* Ask to translate the destination phone
			   number in the Request URI into an IMSI.  */
			newstate = lookup_uri_imsi(&*qmsg);
			set_state(qmsg, newstate);
			break;

		case REQUEST_DESTINATION_SIPURL:
			/* Ask to translate the IMSI in the Request URI
			   into the host/port combo to send it to.  */
			newstate = lookup_uri_hostport(&*qmsg);
			set_state(qmsg, newstate);
			break;

		case AWAITING_TRY_MSG_DELIVERY:
			/* We have waited awhile and now want to try 
			   delivering the message again. */
			set_state(qmsg, REQUEST_MSG_DELIVERY);
			/* No Break */
		case REQUEST_MSG_DELIVERY:
			/* We are trying to deliver to the handset now (or
			   again after congestion).  */
			// debug_dump();	// FIXME, remove
			// Only print delivering msg if delivering to non-
			// localhost.
			if (0 != strcmp("127.0.0.1",
					qmsg->parsed->req_uri->host)) {
				LOG(INFO) << "Delivering '"
				     << qmsg->qtag << "' from "
				     << qmsg->parsed->from->url->username 
				//     << " for "
				//     << qmsg->parsed->req_uri->username
				     << " at "
				     << qmsg->parsed->req_uri->host
				     << ":" << qmsg->parsed->req_uri->port
				     << ".";
			}
			// FIXME, if we can't deliver the datagram we
			// just do the same thing regardless of the result.
			if (my_network.deliver_msg_datagram(&*qmsg))
				set_state(qmsg, REQUEST_DESTINATION_SIPURL);
			else
				set_state(qmsg, REQUEST_DESTINATION_SIPURL);
			break;
			
		case ASKED_FOR_MSG_DELIVERY:
			/* We sent the message to the handset, but never
			   got back an ack.  Must wait awhile to avoid
			   flooding the network or the user with dups. */
			set_state(qmsg, AWAITING_TRY_MSG_DELIVERY);
			break;

		case AWAITING_REGISTER_HANDSET:
			/* We got a shortcode SMS which succeeded in 
			   associating a phone number with this IMSI.
			   Now we have to wait for that to take effect
			   in the HLR, before the next steps. */
			// See if the IMSI maps to a caller ID yet.
			if (!ready_to_register(qmsg)) {
				// Re-up our timeout
				set_state(qmsg, AWAITING_REGISTER_HANDSET);
				break;
			}
			set_state(qmsg, REGISTER_HANDSET);
			/* No Break */
		case REGISTER_HANDSET:
			/* We got a shortcode SMS which succeeded in
			   associating a phone number with this IMSI.
			   Now we have to associate the IMSI with the
			   IP addr:port of its cell site. */
			newstate = register_handset(qmsg);
			set_state(qmsg, newstate);
			break;

		case ASKED_TO_REGISTER_HANDSET:
			/* We got a shortcode SMS which succeeded in
			   associating a phone number with this IMSI.
			   We asked to assoc the IMSI with the addr:port
			   of its cell, but the HLR hasn't answered. */
			// we asked to register; if we time out, go back
			// and try again.
			set_state(qmsg, AWAITING_REGISTER_HANDSET);
			break;
		}

	} /* Repeat from the top. */
}



/*
 * We have received a SIP response message (with a status code like 200,
 * a message like "Okay", and various other fields).  Find the outgoing
 * message that matches this response, and do the right thing.  The response
 * message has already been parsed and validated.
 *
 * If the response says the message is delivered, delete both the response
 * and the message.
 *
 * In general, delete the response so it won't stay at the front of the
 * queue.
 */
void
SMq::handle_response(short_msg_p_list::iterator qmsgit)
{
	short_msg_pending *qmsg = &*qmsgit;
	short_msg_p_list::iterator sent_msg;
	short_msg_p_list resplist;

	// First, remove this response message from the queue.  That way,
	// when we search the queue, we won't find OURSELF.  We also don't
	// want the response hanging around in the queue anyway.
	resplist.splice(resplist.begin(), time_sorted_list, qmsgit);
	// We'll delete the list element on our way out of this function as
	// resplist goes out of scope.

	// figure out what message we're responding to.
	if (!find_queued_msg_by_tag(sent_msg, qmsg->qtag, qmsg->qtaghash)) {
		// No message in queue.
		LOG(NOTICE) << "Couldn't find message for response tag '" 
		     << qmsg->qtag << "'; response is:" << endl
		     << qmsg->text;
		// no big problem, just ignore it.
		return;
	}

	// Check what kind of response we got, based on its status code.
	// FIXME, logfile output would be useful here.
	LOG(NOTICE) << "Got " << qmsg->parsed->status_code
	     << " response for sent msg '" << sent_msg->qtag << "' in state "
	     << sent_msg->state;

	if (   sent_msg->state != ASKED_FOR_MSG_DELIVERY
	    && sent_msg->state != REQUEST_MSG_DELIVERY
	    && sent_msg->state != REQUEST_DESTINATION_SIPURL
	    && sent_msg->state != AWAITING_TRY_MSG_DELIVERY) {
		LOG(ERROR) << "*** That's not a pleasant state. ***";
		// Don't abort here -- if a msg gets forked, one fork
		// gets a redirect/reject, this puts us back into a lookup
		// state, then we get a response from another fork, don't die.
		// Just quietly keep going.
	}
	
	switch (qmsg->parsed->status_code / 100) {
	case 1: // 1xx -- interim response
		// Just ignore it.
		break;

	case 2:	// 2xx -- success.
		// Done!  Extract original msg from queue, and toss it.
		sent_msg->parse();
		if (sent_msg->parsed && 
		    sent_msg->parsed->sip_method && 
		    0 == strcmp("REGISTER", sent_msg->parsed->sip_method)) {
			// This was a SIP REGISTER message, so we need
			// to free up the original SMS Shortcode message that
			// started the registration process.
			short_msg_p_list::iterator oldsms;

			if (!get_link(oldsms, sent_msg)) {
				LOG(NOTICE) << "Can't find SMS message for newly "
					"registered handset, linktag '"
				     << qmsg->linktag << "'.";
				// Assume this was a dup after a retry of
				// the REGISTER message -- thus this is a
				// second REGISTER response, after we already
				// handled the original reg SMS.  Ignore it.
			} else if (
			       oldsms->state == ASKED_TO_REGISTER_HANDSET
			    || oldsms->state == REGISTER_HANDSET
			    || oldsms->state == AWAITING_REGISTER_HANDSET) {
				// Go back in and rerun the SMS message,
				// now that we think we can reply to it.
				// Special code in registration processing
				// will notice it's a re-reg and just reply
				// with a welcome message.
				oldsms->set_state(REQUEST_FROM_ADDRESS_LOOKUP);
			} else {
				// Orig SMS exists, but not in a normal state.
				// Assume that the original SMS is in a
				// retry loop somewhere.  Ignore it.
				// Eventually it'll notice...?  FIXME.
			}
		}

		// Whether a response to a REGISTER or a MESSAGE, delete 
		// the datagram that we sent, which has been responded to.
		LOG(INFO) << "Deleting sent message.";
		resplist.splice(resplist.begin(),
				time_sorted_list, sent_msg);
		resplist.pop_front();	// pop and delete the sent_msg.

		// FIXME, consider breaking loose any other messages for
		// the same destination now.
		break;

	case 4: // 4xx -- failure by client
		// This means the original message was bad.  Bounce it.
		{
			ostringstream errmsg;
			errmsg << qmsg->parsed->status_code << " "
			       << qmsg->parsed->reason_phrase;
			sent_msg->set_state(
			     bounce_message((&*sent_msg), errmsg.str().c_str()));
		}
		break;
		
	case 5: // 5xx -- failure by server (poss. congestion)
		// FIXME, perhaps we should change its timeout value??  Shorter
		// or longer???
		LOG(WARN) << "CONGESTION at OpenBTS\?\?!";
		break;

	case 3: // 3xx -- message needs redirection
	case 6: // 6xx -- message rejected (by this destination).
		// Try going back through looking up the destination again.
		sent_msg->set_state(REQUEST_DESTINATION_IMSI);
		break;

	default:
		LOG(WARN) << "Unknown status code in SIP response.";
		break;
	}
	
	// On exit, we delete the response message we've been examining
	// when resplist goes out of scope.
}

/*
 * Find a queued message, based on its tag value.  Return an iterator
 * that can be used to remove it from the list if desired.
 */
bool
SMq::find_queued_msg_by_tag(short_msg_p_list::iterator &mymsg,
			    const char *tag, int taghash)
{
	short_msg_p_list::iterator x; 

	for (x = time_sorted_list.begin(); x != time_sorted_list.end(); x++) {
		if (taghash == x->qtaghash && !strcmp (tag, x->qtag)) {
			mymsg = x;
		    	return true;
		}
	}
	return false;
}

// Same, but figure out the taghash manually.
bool
SMq::find_queued_msg_by_tag(short_msg_p_list::iterator &mymsg,
			    const char *tag)
{
	return find_queued_msg_by_tag(mymsg, tag, mymsg->taghash_of(tag));
}


/*
 * Validate a short_msg by parsing it and then checking the parse
 * to make sure it has *everything* we need to process and forward it.
 * **ALL** validity checks on incoming short_msg's are centralized here.
 * Thus the rest of the code doesn't need to muck around checking
 * every possible thing -- it can just look at the parts of the message
 * that it needs to.
 *
 * Also, if the qtag hasn't been set yet, set it, based on the message's
 * fields.
 *
 * If we reject a message, return the SIP response code that we should
 * return to the sender, diagnosing the problem.  If the message is OK,
 * result is zero.
 */
int
short_msg_pending::validate_short_msg()
{
	osip_message_t *p;
	__node_t *plist;
	osip_generic_param_t *param;
	char *user;
	size_t clen;
	char *fromtag;
	char *endptr;

    if (print_as_we_validate) {    // debugging
        make_text_valid();
        LOG(DEBUG) << "MSG = " << this->text;
    }

	if (!parsed_is_valid) {
		if (!parse()) {
			return 400;
		}
	}

	/* The message has been parsed.  Now check that we like its 
	   structure and contents.  */

	p = parsed;
	if (!p)			// this should really be an abort.
		return 400;

	if (!p->sip_version || 0 != strcmp("SIP/2.0", p->sip_version))
		return 400;
	// If it's an ack, check some things.  If it's a message,
	// check others.
	if (MSG_IS_RESPONSE(p)) {
		// It's an ack.
		if (p->status_code < 0
		    || !p->reason_phrase)
			return 400;
		clen = 0;
		if (p->content_length && p->content_length->value) {
			errno = 0;
			clen = strtol(p->content_length->value, &endptr, 10);
			if (*endptr != '\0' || errno != 0)
				return 400;
		}
		if (clen != 0)
			return 400;	// Acks have no content!

		if (p->bodies.nb_elt != 0)
			return 400;	// Acks have no bodies!
	} else {
		// It's a message.
		if (!p->req_uri || !p->req_uri->scheme)
			return 400;
		if (0 != strcmp("sip", p->req_uri->scheme))
			return 416;
		if (!check_host_port(p->req_uri->host,p->req_uri->port))
			return 484;
		if (!p->sip_method)
			return 405;

		if (   0 == strcmp("MESSAGE", p->sip_method)) {
			user = p->req_uri->username;
			if (!user)
				return 484;
			if (!check_to_user(user))
				return 484;
			// FIXME, URL versus To: might have different username
			// requirements?

			// FIXME, add support for more Content-Type's.
			if (!p->content_type || !p->content_type->type
			    || !p->content_type->subtype
			    || 0 != strcmp("text", p->content_type->type)
			    || 0 != strcmp("plain", p->content_type->subtype) )
				return 415;

			if (p->bodies.nb_elt != 1 || !p->bodies.node
			    || 0 != p->bodies.node->next
			    || !p->bodies.node->element
			    || false) // ...
				return 413;	// "Request entity-body too large"
			if (!p->cseq || !p->cseq->method
			    || 0 != strcmp("MESSAGE", p->cseq->method)
			    || !p->cseq->number)  // FIXME, validate number??
				return 400;

			// contacts -- cannot occur in SIP MESSAGE's
			// or most response acks.
			if (p->contacts.nb_elt)
				return 400;

		} else if (0 == strcmp("REGISTER", p->sip_method)) {
			// Null username is OK in register message.
			// Null content-type is OK.
			// Null message body also OK.
			if (!p->cseq || !p->cseq->method
			    || 0 != strcmp("REGISTER", p->cseq->method)
			    || !p->cseq->number)  // FIXME, validate number??
				return 400;

		} else {
			return 405;	// Unknown SIP datagram type
		}
	}

	// accepts - no restrictions
	// accept_encodings - no restrictions?
	// accept_langauges - no restrictions?
	// alert_infos - no restrictions?
	// allows - no restrictions?
	// authentication_infos - no restrictions?
	// authorizations - no restrictions
	if (!p->call_id)
		return 400;
	// call_infos - no restrictions
	// content-encodings - FIXME - no restrictions?
	clen = 0;
	if (p->content_length && p->content_length->value) {
		errno = 0;
		clen = strtol(p->content_length->value, &endptr, 10);
		if (*endptr != '\0' || errno != 0)  // Errs or trailing crud?
			return 413;
	}
	// clen is now the numeric content_length.
	// FIXME where is the message body??
	// FIXME check the content_length for < 140 chars too */
	// But we may have to do that AFTER the encoding, etc is decoded

	// error_infos - no restrictions

	// From: needs an extra element which is a message tag?  FIXME if wrong
	if (!p->from || !p->from->url 
	    || p->from->gen_params.nb_elt < 1
	    || !p->from->gen_params.node)
		return 400;
	plist = (__node_t *) p->from->gen_params.node;
	fromtag = NULL;
	do {
		param = (osip_generic_param_t *) plist->element;
  		LOG(DEBUG) << "Param " << param->gname << "=" << param->gvalue;
		if (!strcmp("tag", param->gname))
			fromtag = param->gvalue;
		plist = (__node_t *)plist->next;
	} while (plist);
	if (!fromtag)
		return 400;
	
	if (!p->mime_version) {
		;					// No mime version, OK
	} else if (!p->mime_version->value		// Version 1.0 is OK.
		   || 0 != strcmp("1.0", p->mime_version->value))
		return 415;				// Any other is NOPE.
	// proxy_authenticates - no restrictions
	// proxy_authentication_infos -- no restriction
	// record_routes -- no restriction
	// require -- RFC 3261 sec 20.32: If received, reject this msg!
	//    Unfortunately, the parser doesn't even seem to recognize it!
	// routes -- no restrictions
	if (!p->to || !p->to->url || !p->to->url->scheme
	    || 0 != strcmp("sip", p->to->url->scheme)
	    || !check_host_port(p->to->url->host, p->to->url->port)
	    || !p->to->url->username
#if 0
    // Asterisk returns a tag on the To: line of its SIP REGISTER
    // responses.  This violates the RFC but we'd better allow it.
    // (We don't really care -- we just ignore it anyway.)
	    || p->to->gen_params.nb_elt != 0
#endif
                    )	// no tag field allowed; see
						// RFC 3261 sec 8.1.1.2 
		return 400;
	user = p->to->url->username;
	if (!check_to_user(user))
		return 400;
	// The spec wants Via: line even on acks, but do we care?  FIXME.
	// if (p->vias.nb_elt < 1 || false) // ... FIXME )
	//	return 400;
	// www_authenticates - no restrictions
	// headers - ???
	// message_property ??? FIXME
	// message ??? FIXME
	// message_length ??? FIXME
	// application_data - no restrictions

	// Set the qtag from the parsed fields, if it hasn't been set yet.
	return (set_qtag());
}


// Set the qtag from the parsed fields, if it hasn't been set yet.
// Whenever we change any of these fields, we have to recalculate
// the qtag.  
// FIXME, we assume that the CSEQ, Call-ID, and From tag all are
// components that, in combination, identify the message uniquely.
// FIXME!  The spec is unpleasantly unclear about this.
// Result is 0 for success, or 3-digit integer error code if error.
// FIXME!  The Call-ID changes each time we re-send an SMS.
//  When we get a 200 "Delivered" message for ANY of those
//  different Call-ID's, we should accept that the msg was
//  delivered and delete it.  Therefore, remove the Call-ID
//  from the qtag.
int
short_msg_pending::set_qtag()
{
	osip_message_t *p;
	__node_t *plist;
	osip_generic_param_t *param;
	char *fromtag;

	if (!parsed_is_valid) {
		if (!parse()) {
			return 400;
		}
	}
	p = parsed;

	if (!p->from)
		return 400;
	plist = (__node_t *) p->from->gen_params.node;
	if (!plist)
		return 400;
	fromtag = NULL;
	do {
		param = (osip_generic_param_t *) plist->element;
  		LOG(DEBUG) << "Param " << param->gname << "=" << param->gvalue;
		if (!strcmp("tag", param->gname))
			fromtag = param->gvalue;
		plist = (__node_t *)plist->next;
	} while (plist);
	if (!fromtag)
		return 400;

	if (!p->call_id)
		return 401;
	if (!p->cseq)
		return 402;
	
	delete [] qtag;		// slag the old one, if any.

	int len = strlen(p->cseq->number) + 2	// crlf or --
#ifdef USE_CALL_ID_TAG
		+ strlen(p->call_id->number) + 1 // @
		+ strlen(p->call_id->host) + 2	// crlf or --
#endif
		+ strlen(fromtag) + 1;		// null at end
	qtag = new char[len];
	// There's probably some fancy C++ way to do this.  FIXME.
	strcpy(qtag, p->cseq->number);
	strcat(qtag, "--");
#ifdef USE_CALL_ID_TAG
	strcat(qtag, p->call_id->number);
	strcat(qtag, "@");
	strcat(qtag, p->call_id->host);
	strcat(qtag, "--");
#endif
	strcat(qtag, fromtag);
	// Check the length calculation, abort if bad.
	if (qtag[len-1] != '\0'
	 || qtag[len-2] == '\0')
		abfuckingort();

	// Set the taghash too.  
	// FIXME, if we set this with a good hash function,
	// our linear searches will run much faster, avoiding
	// almost all strcmp's.  For now, punt easy.
	qtaghash = taghash_of(qtag);

	return 0;
}

/* 
 * Hash a tag value for fast searches.
 */
// FIXME, if we set this with a good hash function,
// our linear searches will run much faster, avoiding
// almost all strcmp's.  For now, punt easy.
int
short_msg_pending::taghash_of (const char *fromtag)
{
	return fromtag[0];
}

/* Check the host and port number specified.
 * Currently, be conservative and only take localhost refs.
 * We know FIXME that this will have to be expanded...
 */
bool
short_msg_pending::check_host_port (char *host, char *port)
{
	static int warn_once;

	if (!host)
		return false;
	if (!strcmp ("localhost", host) ||
	    !strcmp ("127.0.0.1", host) ||
	    (smp_my_ipaddress && !strcmp (smp_my_ipaddress, host)) ||
	    (smp_my_2nd_ipaddress && !strcmp (smp_my_2nd_ipaddress, host))) {
		;
	} else {
		if (0 == warn_once++) {
			LOG(NOTICE) << "Accepting SIP Message from " << host <<
			        " for SMS delivery, even though it's not "
				"from localhost.";
		}
		// In theory we should be *routing* it to where it
		// specifies -- but the address is *probably* us, and
		// we have no way to tell what our own address is,
		// since (in a real configuration) we're behind a NAT
		// firewall at an address that never appears in ifconfig,
		// for example.  Must assume it's OK -- for now.  FIXME.
		return true;
	}
	// port can either be specified or not.
	// we could check whether it's all digits...  FIXME

	return true;
}

/*
 * Check the username in the To: field, perhaps in the From: fiend,
 * perhaps in the URI in the MESSAGE line at the top...
 */
bool
check_to_user (char *user)
{
	// For now, don't check -- but port some checks up from the
	// code below in lookup_from_address.  FIXME.
	return true;
}

/* 
 * Make it possible for one message to link to another (by name).
 * We use names rather than pointers because it's independent of the
 * memory management.  Cost = a search of the queue.
 */
// Set the linktag of "newmsg" to point to oldmsg.
void
SMq::set_linktag(short_msg_p_list::iterator newmsg,
		 short_msg_p_list::iterator oldmsg)
{
	if (!oldmsg->qtag) {
		oldmsg->set_qtag();
	}

	size_t len = strlen(oldmsg->qtag);
	newmsg->linktag = new char[1+len];
	strncpy(newmsg->linktag, oldmsg->qtag, len);
	newmsg->linktag[len] = '\0';
}

// Get the other message that this message links to.
// Result is true if found, false if not.
bool
SMq::get_link(short_msg_p_list::iterator &oldmsg,
	      short_msg_p_list::iterator qmsg)
{
	char *alink;

	alink = qmsg->linktag;
	if (!alink)
		return false;
	return find_queued_msg_by_tag(oldmsg, alink);
}


/*
 * Originate half of a short message
 * Put it in the queue and start handling it, but don't actually
 * finish it or send it; return it to the caller for further mucking with.
 *
 * In particular, the caller must set the:
 *    uri
 *    From:
 *    To:
 *    Content-Type and message body, if any.
 *
 * Method is which type of SIP packet (MESSAGE, RESPONSE, etc).
 * Result is a short_msg_p_list containing one short_msg_pending, with
 * the half-initialized message in it.
 */
short_msg_p_list *
SMq::originate_half_sm(string method)
{
	short_msg_p_list *smpl;
	short_msg_pending *response;
	osip_via_t *via;
	char *temp, *p, *mycallnum;
	const char *myhost;

	smpl = new short_msg_p_list (1);
	response = &*smpl->begin();	// Here's our short_msg_pending!
	response->initialize (0, NULL, true);

	osip_message_init(&response->parsed);
	response->parsed_is_valid = true;

	if (!have_register_call_id || method != "REGISTER") {
		// If it's a MESSAGE, or if it's the first REGISTER,
		// it needs a new Call-ID.
		myhost = my_ipaddress.c_str();
		mycallnum = my_network.new_call_number();

		osip_call_id_init(&response->parsed->call_id);
		p = (char *)osip_malloc (strlen(myhost)+1);
		strcpy(p, myhost);
		osip_call_id_set_host (response->parsed->call_id, p);
		p = (char *)osip_malloc (strlen(mycallnum)+1);
		strcpy(p, mycallnum);
		osip_call_id_set_number (response->parsed->call_id, p);
		if (method == "REGISTER") {
			// Save the new call-ID for all subsequent registers
			char *my_callid;
			if (osip_call_id_to_str (response->parsed->call_id,
			                          &my_callid)) {
				abfuckingort();
			}
			register_call_id = string(my_callid);
			osip_free (my_callid);
			register_call_seq = 0;
			have_register_call_id = true;
		}
	} else if (method == "REGISTER") {
		// Copy the saved call-ID
		osip_call_id_init(&response->parsed->call_id);
		osip_call_id_parse(response->parsed->call_id,
				   register_call_id.c_str());
	}

	ostringstream cseqline;
	unsigned int cseq;
	if (method == "REGISTER") {
		cseq = ++register_call_seq;
	} else {
		cseq = my_network.new_random_number();
		cseq &= 0xFFFF;		// for short readable numbers
	}
	cseqline << cseq << " " << method;
	osip_message_set_cseq(response->parsed, cseqline.str().c_str());

	osip_message_set_method (response->parsed, osip_strdup(method.c_str()));

	osip_message_set_via(response->parsed, "SIP/2.0/UDP x 1234;branch=123");
	// FIXME, don't assume UDP, allow TCP here too.
	osip_message_get_via (response->parsed, 0, &via);
	temp = via_get_host(via);
	osip_free(temp);
	via_set_host(via, osip_strdup(my_ipaddress.c_str()));
	temp = via_get_port(via);
	osip_free(temp);
	via_set_port(via, osip_strdup(my_udp_port.c_str()));

	// We've altered the text, and the parsed version controls.
	response->parsed_was_changed();

	// Return our half-baked message (in a list for easy std::list mgmt).
	return smpl;
}


/*
 * Originate a short message
 * Put it in the queue and start handling it.
 * From is a shortcode or phone number or something.
 * To is an IMSI or phone number,
 * msgtext is plain ASCII text.
 * firststate is REQUEST_DESTINATION_IMSI   if to is a phone number.
 *            or REQUEST_DESTINATION_SIPURL if to is an IMSI already.
 * Result is 0 for success, negative for error.
 */
int
SMq::originate_sm(const char *from, const char *to, const char *msgtext,
		enum sm_state firststate)
{
	short_msg_p_list *smpl;
	short_msg_pending *response;
	int errcode;

	smpl = originate_half_sm("MESSAGE");
	response = &*smpl->begin();	// Here's our short_msg_pending!

	// For the tag, we cheat and reuse the cseq number.  
	// I don't see any reason not to...why do we have three different
	// tag fields scattered around?
	ostringstream fromline;
	fromline << from << "<sip:" << from << "@" << my_ipaddress << ">;tag=" 
		 << response->parsed->cseq->number;
	osip_message_set_from(response->parsed, fromline.str().c_str());

	ostringstream toline;
	toline << "<sip:" << to << "@" << my_ipaddress << ">";
	osip_message_set_to(response->parsed, toline.str().c_str());

	ostringstream uriline;
	uriline << "sip:" << to << "@" << my_ipaddress << ":5602";
	osip_uri_init(&response->parsed->req_uri);
	osip_uri_parse(response->parsed->req_uri, uriline.str().c_str());

	osip_message_set_content_type(response->parsed, "text/plain");
    size_t len = strlen(msgtext);
    if (len > SMS_MESSAGE_MAX_LENGTH)
        len = SMS_MESSAGE_MAX_LENGTH;
	osip_message_set_body(response->parsed, msgtext, len);

	// We've altered the text and the parsed version controls.
	response->parsed_was_changed();

	// Now that we set the From tag, we have to create the queue tag.
	response->set_qtag();

	// Now turn it into a text and then parse it for validity
	response->make_text_valid();
	response->unparse();
	
	errcode = response->validate_short_msg();
	if (!errcode) {
		insert_new_message (*smpl, firststate);
	}

	delete smpl;
	return errcode? -1: 0;
}

/*
 * Send a bounce message, based on an existing queued message.
 * Return the state to set the original bouncing message to.
 */
enum sm_state
SMq::bounce_message(short_msg_pending *sent_msg, const char *errstr)
{
	ostringstream errmsg;
	osip_body_t *bod1;
	char *username;
	char *thetext;
	int status;

	username = sent_msg->parsed->to->url->username;
	if (sent_msg->parsed->bodies.nb_elt > 0) {
		// bod1->body is the original SMS text
		bod1 = (osip_body_t *)sent_msg->parsed->bodies.node->element;
		thetext = bod1->body;
	} else {
		thetext = "";
	}

	LOG(NOTICE) << "Bouncing " << sent_msg->qtag << " from "
	     << sent_msg->parsed->from->url->username  // his phonenum
	     << " to " << username << ": " << errstr;

	errmsg << "Can't send your SMS to " << username << ": ";
	if (errstr)
		errmsg << errstr << ": " << thetext;
	else
		errmsg << "can't send: " << thetext;
	
	// Don't bounce a message from 411, makes endless loops.
	status = 1;
	if (0 != strcmp("411", sent_msg->parsed->from->url->username)) {
		// But do bounce anything else.
	        char *bounceto = sent_msg->parsed->from->url->username;
		bool bounce_to_imsi = 0 == strncmp("IMSI", bounceto, 4)
				   || 0 == strncmp("imsi", bounceto, 4);
		status = originate_sm("411", // from "411"? FIXME?
			     bounceto,  // to his phonenum or IMSI
			     errmsg.str().c_str(), // error msg
			     bounce_to_imsi? 
				REQUEST_DESTINATION_SIPURL: // dest is IMSI
			        REQUEST_DESTINATION_IMSI); // dest is phonenum
	}
	if (status == 0) {
	    return DELETE_ME_STATE;
	} else {
	    return NO_STATE;	// Punt to debug.
	}
}

/*
 * See if the handset's imsi and phone number are in the HLR database yet,
 * since if it isn't, we can't register the imsi at its cell's host:port yet.
 */
bool
SMq::ready_to_register (short_msg_p_list::iterator qmsg)
{
	char *callerid, *imsi;
	
	qmsg->parse();
	if (!qmsg->parsed ||
	    !qmsg->parsed->from ||
	    !qmsg->parsed->from->url)
		return false;
	imsi = qmsg->parsed->from->url->username;
	callerid = my_hlr.getCLIDLocal(imsi);
	return (callerid != NULL);
}


/*
 * Register a handset's IMSI with its cell, by sending Asterisk
 * a SIP REGISTER message that we "relay" from the cell.
 * (We actually originate it, but we pretend that the cell sent it to us.
 *  Actually the cell sent us an incoming shortcode SMS from an
 *  unregistered phone, which is in the queue in REGISTER_HANDSET state.)
 *
 * Argument qmsg is the SMS message, with its From line still an IMSI.
 * We register "IMSI@HLRhost:HLRport" at the sip uri:
 *             "IMSI@cellhost:cellport".
 */
enum sm_state
SMq::register_handset (short_msg_p_list::iterator qmsg)
{
	short_msg_p_list *smpl;
	short_msg_pending *response;
	int errcode;
	char *imsi;

	smpl = originate_half_sm("REGISTER");
	response = &*smpl->begin();	// Here's our short_msg_pending!

	imsi = qmsg->parsed->from->url->username;

	// The To: line is the long-term name being registered.
	ostringstream toline;
	toline << imsi << "<sip:" << imsi << "@" << my_register_hostport << ">";
	osip_message_set_to(response->parsed, toline.str().c_str());

	// The From: line is the same, plus a tag.
	// However, we steal the tag from our cseq, since we don't care
	// about it much.
	ostringstream fromline;
	fromline << toline.str() << ";tag=" << response->parsed->cseq->number;
	osip_message_set_from(response->parsed, fromline.str().c_str());

	// URI in the first line: insert SIP HLR's host/port.
	ostringstream uriline;
	uriline << "sip:" << my_register_hostport;
	osip_uri_init(&response->parsed->req_uri);
	osip_uri_parse(response->parsed->req_uri, uriline.str().c_str());

	// Contact: field specifies where we're registering from.
	ostringstream contactline;
	contactline << "<sip:" << imsi << "@";
	contactline << my_network.string_addr((struct sockaddr *)qmsg->srcaddr,
					      qmsg->srcaddrlen, true);
	contactline << ">;expires=3600";
	osip_message_set_contact(response->parsed, contactline.str().c_str());

	// We've altered the fields, and the parsed version controls.
	response->parsed_was_changed();

	// Now that we set the From tag, we have to create the queue tag.
	response->set_qtag();

	// Set the linktag of our REGISTER message to point to qmsg (SMS msg)
	set_linktag(smpl->begin(), qmsg);

	// Now turn it into a text and then parse it for validity
	response->make_text_valid();
	response->unparse();
	
	errcode = response->validate_short_msg();
	if (errcode) {
		abfuckingort();		// Our msg should be valid
	}

	// Pop new SIP REGISTER out of the smpl queue-of-one and
	// into the real queue, where it will very soon be delivered.
	insert_new_message (*smpl, REQUEST_MSG_DELIVERY);
	// We can't reference response, or *smpl, any more...

	delete smpl;

	// The next state of the original (SMS shortcode) message is...
	return ASKED_TO_REGISTER_HANDSET;
}


/*
 * Initial handling of SMS messages found in the queue
 */
enum sm_state
SMq::handle_sms_message (short_msg_p_list::iterator qmsg)
{
	osip_body_t *bod1;
	char *bods;
	short_func_t shortfn;
	short_code_map_t::iterator shortit;
	enum short_code_action sca;
	short_code_params params;
	int status;

	if (0 != strcmp(qmsg->parsed->sip_method, "MESSAGE")) {
		LOG(WARN) << "Invalid incoming SIP message, method is "
		     << qmsg->parsed->sip_method;
		return NO_STATE;
	}
	
	shortit = short_code_map.find (string(qmsg->parsed->req_uri->username));

	if (shortit != short_code_map.end()) {
		/* Messages to certain addresses are special commands */
		shortfn = shortit->second;
		bods = (char *)"";
	        if (qmsg->parsed->bodies.nb_elt == 1) {
			bod1 = (osip_body_t *)qmsg->parsed->bodies.node->element;
			bods = bod1->body;
		}

		// Set up arguments and access pointers, then call
		// the short-code function to process it.
		params.scp_retries = qmsg->retries;
		params.scp_smq = this;
		params.scp_qmsg_it = qmsg;

		LOG(INFO) << "Short-code SMS "
		     << qmsg->parsed->req_uri->username
		     << "(" << bods << ").";
		
		sca = (*shortfn) (qmsg->parsed->from->url->username, // imsi
				bods,  // msg text
				&params);

		// The short-code function asks us to do something when
		// it's done.  Do it.
		switch (sca) {
		case SCA_REPLY:
			LOG(INFO) << "Short-code replies: "
			     << params.scp_reply;
			status = originate_sm(
			  qmsg->parsed->req_uri->username,  // from shortcode
			  qmsg->parsed->from->url->username,// to his IMSI
			  params.scp_reply, REQUEST_DESTINATION_SIPURL);
			if (!status) {
				return DELETE_ME_STATE;	 // Done!
			}
			LOG(NOTICE) << "Reply failed " << status << "!";
			// NO BREAK
		default:
		case SCA_INTERNAL_ERROR:
			LOG(ERROR) << "Error in short-code function "
			     << qmsg->parsed->req_uri->username
			     << "(" << bods << "): " << params.scp_reply;
			return NO_STATE;

		case SCA_EXEC_SMQUEUE:
			reexec_smqueue = true;
			stop_main_loop = true;
			return DELETE_ME_STATE;
			
		case SCA_QUIT_SMQUEUE:
			stop_main_loop = true;
			return DELETE_ME_STATE;

		case SCA_DONE:
			return DELETE_ME_STATE;		

		case SCA_RETRY_AFTER_DELAY:
			// FIXME, timeout is implicit in set_state table,
			// rather than taken from params.scp_delay.
			qmsg->retries++;
			return REQUEST_FROM_ADDRESS_LOOKUP;

		case SCA_AWAIT_REGISTER:
			// We just linked the phone# to the IMSI, but we
			// have to wait til the HLR updates, before
			// we can link the IMSI to the originating IP address
			// and port number of its cell.
			return AWAITING_REGISTER_HANDSET;

		case SCA_REGISTER:
			return register_handset(qmsg);

		case SCA_TREAT_AS_ORDINARY:
			break;		// fall through into non-special case.
		}
	}

	// For non-special messages, look up who they're from.
	return lookup_from_address (&*qmsg);
}


/* Change the From address to a valid phone number in + countrycode phonenum
   format.  Also add a Via: line about us.  */
enum sm_state
SMq::lookup_from_address (short_msg_pending *qmsg)
{
	char *host = qmsg->parsed->from->url->host;
	bool got_phone = false;

	char *scheme = qmsg->parsed->from->url->scheme;
	if (!scheme) return NO_STATE;
	if (0 != strcmp("sip", scheme)) return NO_STATE;

	char *username = qmsg->parsed->from->url->username;
	if (!username) return NO_STATE;
	
	if (!host) return NO_STATE;
	if (0 != strcmp("127.0.0.1", host)
 	 && 0 != strcmp("localhost", host)
 	 && 0 != strcmp(my_ipaddress.c_str(), host)
 	 && 0 != strcmp(my_2nd_ipaddress.c_str(), host)) {
		// This isn't a phone number.
		//
		// There's a bizarre convention to move the email address
		// into the message text, followed by a space... GSM 03.40 sec 3.8
		ostringstream newtext;
		osip_body_t *bod1;
		char *bods;

		bods = (char *)"";
	        if (qmsg->parsed->bodies.nb_elt == 1) {
			bod1 = (osip_body_t *)qmsg->parsed->bodies.node->element;
			bods = bod1->body;
		} else {
			return NO_STATE;		// Punt on msg w/no text
		}
		newtext << qmsg->parsed->from->url->username << "@" 
			<< host << " " << bods;
		osip_free(bods);
		bod1->body = osip_strdup(newtext.str().c_str());
		bod1->length = strlen(bod1->body);

		// Change From address to a local shortcode
		osip_free(qmsg->parsed->from->url->username);
		osip_free(qmsg->parsed->from->url->host);
		qmsg->parsed->from->url->username = osip_strdup ("211");
		qmsg->parsed->from->url->host = osip_strdup (my_ipaddress.c_str());
		got_phone = true;
	}

	// Insert a Via: line describing us, this makes us easier to trace,
	// and also allows a remote SIP agent to reply to us.  (Maybe?)
	osip_via_t *via;
	char *temp;
	osip_message_append_via(qmsg->parsed, "SIP/2.0/UDP x:1234;branch=123");
	// FIXME, don't assume UDP, allow TCP here too.
	osip_message_get_via (qmsg->parsed, 0, &via);
	temp = via_get_host(via);
	osip_free(temp);
	via_set_host(via, osip_strdup(my_ipaddress.c_str()));
	temp = via_get_port(via);
	osip_free(temp);
	via_set_port(via, osip_strdup(my_udp_port.c_str()));

	/* Username can be in various formats.  Check for formats that
	   we know about.  Anything else we punt.  */
	if (got_phone || username[0] == '+' || isdigit(username[0])) {
		/* We have a phone number.  This is what we want.
		   So we're done, and can move on to the next part
		   of processing the short_msg. */
		return REQUEST_DESTINATION_IMSI;
	}

	/* If we have "imsi" on the front, strip it.  */
	char *tryuser = username;
	if ((username[0] == 'i'||username[0]=='I')
         && (username[1] == 'm'||username[1]=='M')
	 && (username[2] == 's'||username[2]=='S')
	 && (username[3] == 'i'||username[3]=='I')) {
		tryuser += 4;
	}

	/* http://en.wikipedia.org/wiki/International_Mobile_Subscriber_Identity */
	size_t len = strlen (tryuser);
        if (len != 15 && len != 14) {
		/* This is not an IMSI.   Punt.  */
		return NO_STATE;
	}

	/* Look up the IMSI in the Home Location Register. */
	char *newfrom;

	newfrom = my_hlr.getCLIDLocal(username);
	if (!newfrom) {
		/* ==================FIXME KLUDGE====================
		 * Here is our fake table of IMSIs and phone numbers
		 * ==================FIXME KLUDGE==================== */
		for (int i = 0; imsi_phone[i].imsi[0]; i++) {
			if (0 == strcmp(imsi_phone[i].imsi, username)) {
				newfrom = strdup(imsi_phone[i].phone);
				break;
			}
		}
	}

	if (!newfrom) {
		LOG(NOTICE) << "Lookup IMSI <" << username
		     << "> to phonenum failed.";
		LOG(DEBUG) << qmsg->text;
		return bounce_message (qmsg,
			gConfig.getStr("BounceMessage.IMSILookupFailed")
		);
		// return NO_STATE;	// Put it into limbo for debug.
	}

	/* We found it!  Translation done! 
	   Now the dance of freeing the old name and
	   inserting new name.  */
	char *p;

	osip_free (qmsg->parsed->from->url->username);
	p = (char *)osip_malloc (strlen(newfrom)+1);
	strcpy(p, newfrom);
	qmsg->parsed->from->url->username = p;
	qmsg->parsed_was_changed();

	free(newfrom);		// C interface uses free() not delete.
	return REQUEST_DESTINATION_IMSI;
}

/* Change the Request-URI's address to a valid IMSI.  */
enum sm_state
SMq::lookup_uri_imsi (short_msg_pending *qmsg)
{
	qmsg->parse();

	char *scheme = qmsg->parsed->req_uri->scheme;
	if (!scheme) return NO_STATE;
	if (0 != strcmp("sip", scheme)) return NO_STATE;

	char *host = qmsg->parsed->req_uri->host;
	if (!host) return NO_STATE;
	if (0 != strcmp("127.0.0.1", host)
 	 && 0 != strcmp("localhost", host)
 	 && 0 != strcmp(my_ipaddress.c_str(), host)
 	 && 0 != strcmp(my_2nd_ipaddress.c_str(), host))
		return NO_STATE;

	char *username = qmsg->parsed->req_uri->username;
	if (!username) return NO_STATE;
	
	/* Username can be in various formats.  Check for formats that
	   we know about.  Anything else we punt.  */
	if (username[0] == '+' || (   0 != strncmp("imsi", username, 4)
				   && 0 != strncmp("IMSI", username, 4))) {
		// We have a phone number.  It needs translation.

		char *newdest = my_hlr.getIMSI(username);
		if (!newdest) {
			/* ==================FIXME KLUDGE====================
			 * Here is our fake table of IMSIs and phone numbers
			 * ==================FIXME KLUDGE==================== */
			for (int i = 0; imsi_phone[i].phone[0]; i++) {
				if (0 == strcmp(imsi_phone[i].phone, username)) {
					newdest = strdup(imsi_phone[i].imsi);
					break;
				}
			}
		}

		// It had better say "imsi" if it's an IMSI, else
		// lookup_uri_hostport will get confused.
		if (newdest
                 && 0 != strncmp("imsi", newdest, 4) 
		 && 0 != strncmp("IMSI", newdest, 4)) {
			free(newdest);
			newdest = NULL;
		}

		if (!newdest) {
			/* Didn't find it in HLR or fake table.  Bitch. */
			// We have to return an error to the originator.
			LOG(NOTICE) << "Lookup phonenum '" << username << "' to IMSI failed.";
			LOG(DEBUG) << "MSG = " << qmsg->text;

		    if (global_relay.c_str()[0] == '\0'
			|| !my_hlr.useGateway(username)) {
			// There's no global relay -- or the HLR says not to
			// use the global relay for it -- so send a bounce.
			return bounce_message (qmsg, gConfig.getStr("BounceMessage.NotRegistered"));
		    } else {
			// Send the message to our global relay.
			// We leave the username as a phone number, and
			// let it pass on to look up the destination
			// SIP URL (which is the global relay).
			//
			// However, the From address is at this point the
			// sender's local ph#.  Map it to the global ph#.
			char *newfrom;
			newfrom = my_hlr.mapCLIDGlobal(
					qmsg->parsed->from->url->username);
			if (newfrom) {
				osip_free(qmsg->parsed->from->url->username);
				qmsg->parsed->from->url->username = 
					osip_strdup (newfrom);
			}
			return REQUEST_DESTINATION_SIPURL;
		    }
		}

		/* We found it!  Translation done! 
		   Now the dance of freeing the old name and
		   inserting new name.  */
		char *p;
		osip_free (qmsg->parsed->req_uri->username);
		p = (char *)osip_malloc (strlen(newdest)+1);
		strcpy(p, newdest);
		qmsg->parsed->req_uri->username = p;
		qmsg->parsed_was_changed();

		free(newdest);		// C interface uses free() not delete

		return REQUEST_DESTINATION_SIPURL;
	}

	/* If we have "imsi" on the front, scan past it.  */
	if (username[0] == 'i' && username[1] == 'm'
	 && username[2] == 's' && username[3] == 'i') {
		username += 4;
	}

	/* http://en.wikipedia.org/wiki/International_Mobile_Subscriber_Identity */
	size_t len = strlen (username);
        if (len != 15 && len != 14) {
		/* This is not an IMSI.   Punt.  */
		return NO_STATE;
	}

	/* We have an IMSI already.  Now figure out how to route it. */
	return REQUEST_DESTINATION_SIPURL;
}


/*
 * Look up the hostname and port number where we should send Short Messages
 * for the IMSI (or phone number, if we're using a global relay)
 * in the To address.  
 * 
 * This is also where we assign a new Call-ID to the message, so that
 * re-sends will use the same Call-ID, but re-locate's (looking up the
 * recipient's location again) will use a new one.
 */
enum sm_state
SMq::lookup_uri_hostport (short_msg_pending *qmsg)
{

	qmsg->parse();

	char *imsi = qmsg->parsed->req_uri->username;
	char *p, *mycallnum; 
	char *newhost, *newport;
	const char *myhost; 

	if (!imsi) return NO_STATE;

	/* Username can be in various formats.  Check for formats that
	   we know about.  Anything else we punt.  */
	if (imsi[0] == '+' || (0 != strncmp("imsi", imsi, 4)
			    && 0 != strncmp("IMSI", imsi, 4))) {
		// We have a phone number.  It needs translation.
		newport = NULL;
		newhost = strdup(global_relay.c_str());
	} else {
		/* imsi is an IMSI at this point.  */

		newport = NULL;
		newhost = my_hlr.getRegistrationIP (imsi);
	}

	if (newhost) {
		// Break up returned "host:port" string.
		char *colon = strchr(newhost,':');
		if (!colon) {
			free(newhost);
			newhost = NULL;
			newport = NULL;
		} else {
			newport = strdup(colon+1);
			*colon = '\0';
		}
	}

	// KLUDGE! KLUDGE! KLUDGE! for testing only
	if (!newhost) {
		newhost = strdup((char *)"127.0.0.1");
	}
	if (!newport) {
		newport = strdup((char *)"5062");
	}

	/* We found it!  Translation done! 
	   Now the dance of freeing the old ones and
	   inserting new ones.  */

	if (0 != strcmp (newhost, qmsg->parsed->req_uri->host))
	{
		osip_free (qmsg->parsed->req_uri->host);
		p = (char *)osip_malloc (strlen(newhost)+1);
		strcpy(p, newhost);
		qmsg->parsed->req_uri->host = p;
		qmsg->parsed_was_changed();
	}

	if (qmsg->parsed->req_uri->port != newport)
	{
		osip_free (qmsg->parsed->req_uri->port);
		p = newport;
		if (newport) {
			p = (char *)osip_malloc (strlen(newport)+1);
			strcpy(p, newport);
		}
		qmsg->parsed->req_uri->port = p;
		qmsg->parsed_was_changed();
	}

	// We've altered the message, it's a new message, and it needs
	// a new Call-ID so it won't be confused with the old message.
	myhost = my_ipaddress.c_str();
	mycallnum = my_network.new_call_number();

	if (!qmsg->parsed->call_id) {
		osip_call_id_init(&qmsg->parsed->call_id);
	}

	if (0 != strcmp(myhost, 
			osip_call_id_get_host (qmsg->parsed->call_id))) {
		osip_free (osip_call_id_get_host (qmsg->parsed->call_id));
		p = (char *)osip_malloc (strlen(myhost)+1);
		strcpy(p, myhost);
		osip_call_id_set_host (qmsg->parsed->call_id, p);
		qmsg->parsed_was_changed();
	}

	if (0 != strcmp(mycallnum,
		 	osip_call_id_get_number (qmsg->parsed->call_id))) {
		osip_free (osip_call_id_get_number (qmsg->parsed->call_id));
		p = (char *)osip_malloc (strlen(mycallnum)+1);
		strcpy(p, mycallnum);
		osip_call_id_set_number (qmsg->parsed->call_id, p);
		qmsg->parsed_was_changed();
	}

	// Now that we changed the Call-ID, we have to update the queue tag.
	qmsg->set_qtag();

	// Both of these were dynamic storage; don't leak them.
	// (They were allocated by malloc() so we free with free().
	//  Note that osip_malloc isn't necessarily malloc -- and 
	//  neither of them is necessarily  new  or  delete .)
	free(newhost);
	free(newport);

	// OK, we're done; next step is to deliver it to that host & port!
	return REQUEST_MSG_DELIVERY;
}

/*
 * Helper function because C++ is fucked about types.
 * and the osip library doesn't keep its types straight.
 */
int
osip_via_clone2 (void *via, void **dest)
{
	return osip_via_clone ((const osip_via_t *)via, 
			       (osip_via_t **)dest);
}


/*
 * After we received a datagram, send a SIP response message
 * telling the sender what we did with it.  (Unless the datagram we
 * received was already a SIP response message...)
 */
void 
SMq::respond_sip_ack(int errcode, short_msg_pending *smp, 
		char *netaddr, size_t netaddrlen)
{
	string phrase;
	short_msg response;
	bool okay;

	if (!smp->parse())
		return;		// Don't ack invalid SIP messages.

	if (MSG_IS_RESPONSE(smp->parsed))
		return;		// Don't ack a response message, or we loop!

	osip_message_init(&response.parsed);
	response.parsed_is_valid = true;

	// Copy over the CSeq, From, To, Call-ID, Via, etc.
	osip_to_clone(smp->parsed->to,     &response.parsed->to);
	osip_from_clone(smp->parsed->from, &response.parsed->from);
	osip_cseq_clone(smp->parsed->cseq, &response.parsed->cseq);
	osip_call_id_clone(smp->parsed->call_id, &response.parsed->call_id);
	osip_list_clone(&smp->parsed->vias, &response.parsed->vias, 
			&osip_via_clone2);

	// Start a new Via: line.
	ostringstream newvia;
	// FIXME, don't assume UDP, allow TCP here too.
	newvia << "SIP/2.0/UDP " << my_ipaddress << ":" << my_udp_port
	       << ";branch=1;received=" 
	       << my_network.string_addr((struct sockaddr *)netaddr, netaddrlen,
					 false);
	osip_message_append_via(response.parsed, newvia.str().c_str());

	// Make a nice message.
	switch (errcode) {
	case 100:	phrase="Trying..."; break;
	case 200:	phrase="Okay!"; break;
	case 202:	phrase="Queued"; break;
	case 400:	phrase="Bad Request"; break;
	case 401:	phrase="Un Author Ized"; break;
	case 403:	phrase="Forbidden - first register, by texting your 10-digit phone number to 101."; break;
	case 404:	phrase="Phone Number Not Registered"; break;  // Not Found
	case 405:	phrase="Method Not Allowed";
			osip_message_set_allow(response.parsed, "MESSAGE");
			break;
	case 413:	phrase="Message Body Size Error"; break;
	case 415:	phrase="Unsupported Content Type";
			osip_message_set_accept(response.parsed, "text/plain");
			break;
	case 416:	phrase="Unsupported URI scheme (not SIP)"; break;
	case 480:	phrase="Recipient Temporarily Unavailable"; break;
	case 484:	phrase="Address Incomplete"; break;
	default:	phrase="Error Message Table Needs Updating"; break;
	}
		
	osip_message_set_status_code (response.parsed, errcode);
	osip_message_set_reason_phrase (response.parsed,
				        osip_strdup((char *)phrase.c_str()));

	// We've altered the text and the parsed version controls.
	response.parsed_was_changed();

	// Now turn it into a datagram and hustle it home.
	response.make_text_valid();
	LOG(INFO) << "Responding with \"" << errcode << " " << phrase << "\".";
	
	okay = my_network.send_dgram(response.text, strlen(response.text),
				     netaddr, netaddrlen);
	if (!okay)
		LOG(ERROR) << "send_dgram had trouble sending the response.";
}

//
// The main loop that listens for incoming datagrams, handles them
// through the queue, and moves them toward transmission.
//
void
SMq::main_loop()
{
	int len;		// MUST be signed -- not size_t!
				// else we can't see -1 for errors...
	int timeout, mstimeout;
	short_msg_p_list *smpl;
	short_msg_pending *smp;
	char buffer[5000];
	short_msg_p_list::iterator qmsg;
	time_t now;
	int errcode;

	stop_main_loop = false;

   while (!stop_main_loop) {

	now = time(NULL);		
	qmsg = time_sorted_list.begin();
	if (qmsg == time_sorted_list.end()) {
		timeout = -1;			// Infinite timeout
	} else {
		timeout = qmsg->next_action_time - now;
		if (timeout < 0) timeout = 0;  // Check for incoming anyway
	}
	mstimeout = 1000 * timeout;

#undef DEBUG_Q
#ifdef DEBUG_Q
	LOG(DEBUG) << "=== Top of main_loop: queue:";
	debug_dump();
	LOG(DEBUG) << "============== End of queue.  timeout = " << timeout;
#else
	char timebuf[26+/*slop*/4];	// 
	ctime_r(&now, timebuf);
	timebuf[19] = '\0';	// Leave out space, year and newline

	if (timeout < 0) {
	    LOG(INFO) << "=== " << timebuf+4 << " "
	     << time_sorted_list.size() << " queued; "
		<< "waiting.";
	} else {
	    LOG(INFO) << "=== " << timebuf+4 << " "
	     << time_sorted_list.size() << " queued; "
		     << timeout << " seconds til "
		     << sm_state_string(qmsg->state)
		     << " for " << qmsg->qtag;
	}
#endif
	len = my_network.get_next_dgram(buffer, sizeof(buffer), mstimeout);	

	if (len < 0) {
		// Error.
		LOG(ERROR) << "Error from get_next_dgram: " << strerror(errno);
		// Just continue...
	} else if (len == 0) {
		// Timeout.  Just push things along.
		LOG(DEBUG) << "Timeout...";
	} else {
		// We got a datagram.  Dump it into the queue, copying it.
		//
		// Here we do a bit of tricky memory allocation.  Rather
		// than make a short_msg_pending and then have to COPY it
		// into a short_msg_p_list (including malloc-ing all the
		// possible pointed-to stuff and then freeing all the original
		// strings and things), we make a short_msg_p_list
		// and create in it a single default element.  Then we fill
		// in that element as our new short_msg_pending.  This lets
		// us (soon) link it into the main message queue list, 
		// without ever copying it.
		// 
		// HOWEVER!  The implementation of std::list in GNU C++
		// (ver. 4.3.3) has a bug: it does not PERMIT a class to be
		// made into
		// a list UNLESS it allows copy-construction of its instances.
		// You get an extremely inscrutable error message deep
		// in the templated bowels of stl_list.h , referencing
		// the next non-comment line of this file.
		// THUS, we can't check at compile time to prevent the
		// making of copies of short_msg_pending's -- instead, we
		// have to do that check at runtime (allowing the default
		// newly-initialized one to be copied, but aborting with
		// any depth of stuff in it).
		smpl = new short_msg_p_list (1);
		smp = &*smpl->begin();	// Here's our short_msg_pending!
		smp->initialize (len, buffer, false);

		if (my_network.recvaddrlen <= sizeof (smp->srcaddr)) {
			smp->srcaddrlen = my_network.recvaddrlen;
			memcpy(smp->srcaddr, my_network.src_addr, 
			       my_network.recvaddrlen);
		}

		errcode = smp->validate_short_msg();
		if (errcode == 0) {
			if (MSG_IS_REQUEST(smp->parsed)) {
				LOG(NOTICE) << "Got SMS '"
				     << smp->qtag << "' from "
				     << smp->parsed->from->url->username 
				     << " for "
				     << smp->parsed->req_uri->username
				     << ".";
			} else {
				LOG(INFO) << "Got SMS "
				     << smp->parsed->status_code
				     << " Response '"
				     << smp->qtag << "'.";
			}
			insert_new_message (*smpl);
			errcode = 202;	// Accepted and queued.
		} else {
			LOG(WARN) << "Received bad " << errcode
			     << " datagram:" << endl
	                     << "BADMSG = " << smp->text;
		}
		// It's OK to reference "smp" here, whether it's in the
		// smpl list, or has been moved into the main time_sorted_list.
		respond_sip_ack (errcode, smp, smp->srcaddr, smp->srcaddrlen);

		// We won't leak memory if we didn't queue it up, since
		// the delete of smpl will delete anything still
		// in ITS list.
		delete smpl;
	}

	process_timeout();
    } /* while (!stop_main_loop) */
}


/* Debug dump of SMq and mainly the queue. */
void SMq::debug_dump() {
	short_msg_p_list::iterator x = time_sorted_list.begin();
	time_t now = time(NULL);
	for (; x != time_sorted_list.end(); ++x) {
		x->make_text_valid();
		LOG(DEBUG) << "== State: " << sm_state_string (x->state) << "\t"
		     << (x->next_action_time - now) << endl << "MSG = "
		     << x->text;
	}
}

/* Print net addr in hex.  Returns a static buffer.  */
char *
netaddr_fmt(char *srcaddr, unsigned len)
{
	static char buffer[999];
	char *bufp = buffer;

	buffer[0] = 0;
	*bufp++ = '=';
	while (len > 0) {
		snprintf(bufp, 3, "%02x", *(unsigned char *)srcaddr);
		bufp+= 2;
		len--;
	}
	return buffer;
}

/* Parse a string of hex into a net address and length */
bool
netaddr_parse(const char *str, char *addr, unsigned int *len)
{
	if (str[0] != '=') 
		return false;
	str++;
	int xlen = strlen(str);
	int retlen = xlen/2;
	if (xlen != retlen*2)
		return false;
	*len = retlen;
	char *myaddr = addr;
	const char *strp = str;
	while (retlen > 0) {
		char blah[3];
		unsigned int mybyte;
		blah[0] = strp[0]; blah[1] = strp[1]; blah[2] = '\0';
		sscanf(blah, "%x", &mybyte);
		*myaddr++ = mybyte;
		strp+= 2;
		retlen--;
	}
	return true;
}

/*
 * Save queue to file.
 * 
 * We save in reverse timestamp order, to make it very fast to insert when
 * re-read.
 */
bool
SMq::save_queue_to_file(std::string qfile)
{
	short_msg_p_list::reverse_iterator x = time_sorted_list.rbegin();
	ofstream ofile;
	unsigned howmany = 0;

	ofile.open(qfile.c_str(), ios::out | ios::binary | ios::trunc);
	if (!ofile.is_open())
		return false;
	for (; x != time_sorted_list.rend(); ++x) {
		x->make_text_valid();
		ofile << "=== " << (int) x->state << " " 
		      << x->next_action_time << " "
		      << my_network.string_addr((struct sockaddr *)x->srcaddr, x->srcaddrlen, true) << " "
		      << strlen(x->text) << endl << x->text << endl << endl;
		howmany++;
	}
  
	bool result = !ofile.fail();
	ofile.close();
	if (result) {
		LOG(INFO) << "Saved " << howmany << " queued messages to " << qfile;
	} else {
		LOG(ERROR) << "FAILED to save " << howmany << " queued messages to " << qfile;
	}
	return result;
}

/*
 * Read a new queue from file.
 */
bool
SMq::read_queue_from_file(std::string qfile)
{
	ifstream ifile;
	std::string equals;
	unsigned astate, atime, alength;
	std::string netaddrstr;
	sm_state mystate;
	time_t mytime;
	char *msgtext;
	unsigned howmany = 0, howmanyerrs = 0;
	char ignoreme;
	short_msg_p_list *smpl;
	short_msg_pending *smp;
	int errcode;

	ifile.open (qfile.c_str(), ios::in | ios::binary);
	if (!ifile.is_open())
		return false;
	while (!ifile.eof()) {
		ifile >> equals >> astate >> atime;
		if (equals != "===") {
			if (ifile.eof())
				break;
			abfuckingort();
		}
		ifile >> netaddrstr;
		ifile >> alength;
		while (ifile.peek() == '\n')
			ignoreme = ifile.get();
		msgtext = new char[alength+2];
		// Get alength chars (or until null char, hope there are none)
		ifile.get(msgtext, alength+1, '\0');
		while (ifile.peek() == '\n')
			ignoreme = ifile.get();
		howmany++;
		mystate = (SMqueue::sm_state)astate;
		mytime = atime;
		
		smpl = new short_msg_p_list (1);
		smp = &*smpl->begin();	// Here's our short_msg_pending!
		smp->initialize (alength, msgtext, true);
		// We use the just-allocated msgtext; it gets freed after
		// delivery of message.

		smp->srcaddrlen = 0;
		if (!my_network.parse_addr(netaddrstr.c_str(), smp->srcaddr, sizeof(smp->srcaddr), &smp->srcaddrlen))
			abfuckingort();

		errcode = smp->validate_short_msg();
		if (errcode == 0) {
			if (MSG_IS_REQUEST(smp->parsed)) {
				LOG(INFO) << "Read SMS '"
				     << smp->qtag << "' from "
				     << smp->parsed->from->url->username 
				     << " for "
				     << smp->parsed->req_uri->username
				     << ".";
			} else {
				LOG(WARN) << "Read bad SMS "
				     << smp->parsed->status_code
				     << " Response '"
				     << smp->qtag << "':" << msgtext;
			}
			insert_new_message (*smpl, mystate, mytime);
		} else {
			LOG(ERROR) << "Read bad " << errcode
			     << " message:" << endl
	                     << "BADMSG = " << smp->text;
			howmanyerrs++;
		}
		delete smpl;
	}
	LOG(INFO) << "=== Read " << howmany << " messages total, " << howmanyerrs
	     << " bad ones.";
	ifile.close();
	return true;
}


/*
 * Read in a message from a file.  Return malloc'd char block of the whole
 * thing.
 */
char *
read_msg_text_from_file(char *fname, size_t *lptr)
{
	ifstream ifs;
	size_t length, got;
	char *mess;
	int i;

	ifs.open (fname, ios::in | ios::binary | ios::ate);
	if (!ifs.is_open())
		return NULL;
	length = ifs.tellg();	// we opened it at the end
	mess = new char[length+1];
	if (!mess) {
		ifs.close();
		*lptr = length;		// may as well return the length
		return NULL;		// even though we fail.
	}
	ifs.seekg (0, ios::beg);	// Now go back to the beginning.
	got = ifs.readsome (mess, length+1);
	mess[length] = '\0';		// terminate the string
	i = ifs.eof();			// We should be at EOF.
		// but due to bugs in iostreams, we aren't.  FIXME.
	i = ifs.bad();			// But we shouldn't have bad().
	ifs.close();
	if (i || got != length)
		return NULL;
	*lptr = length;
	return mess;
}


// FIXME - this needs work to adjust to the new insert_new_message
// memory allocation paradigm.
short_msg_pending *
read_short_msg_pending_from_file(char *fname)
{
	short_msg_pending *smp;
	size_t length;
	char *sip_text;

	sip_text = read_msg_text_from_file (fname, &length);
	if (!sip_text)
		return NULL;

	// FIXME!  We should also be able to read the state, timeout, etc.
	// FIXME!  This'll be needed for writing/restoring the queue.
	
	smp = new short_msg_pending (length, sip_text, true);
	return smp;
}



/* Really simple first try */

int
main(int argc, char **argv)
{
  bool please_re_exec = false;
  // short_msg_p_list aq;
  // short_msg *sm;
  // short_msg_pending *smp;
  std::string savefile;

  init_smcommands(&short_code_map); // Set up short-code commands users can type

  // This scope lets us delete the smq (and the network sockets)
  // before we re-exec ourself.
  while (true) {
    SMq smq;			/* Our big state machine & msg queue */

    // Configure the logger.
    if (gConfig.defines("Log.FileName")) {
      gSetLogFile(gConfig.getStr("Log.FileName"));
    }

    // IP address:port of the Home Location Register that we send SIP
    // REGISTER messages to.
    smq.set_register_hostport(gConfig.getStr("Asterisk.address"));  

    // IP address:port of the global relay where we sent SIP messages
    // if we don't know where else to send them.
    smq.set_global_relay(gConfig.getStr("SIP.global_relay"));

    // IP address of our own smqueue, as seen from outside.
    smq.set_my_ipaddress(gConfig.getStr("SIP.myIP" /* "127.0.0.1" */));
    smq.set_my_2nd_ipaddress(gConfig.getStr("SIP.myIP2" /* "NAT crap" */));

    // Port number that we (smqueue) listen on.
    smq.init_listener(gConfig.getStr("SIP.myPort"));	// Port number to listen on.

    // Debug -- print all msgs in log
  //  print_as_we_validate = gConfig.getBool("Debug.print_as_we_validate");
    print_as_we_validate = gConfig.defines("Debug.print_as_we_validate");

    // system() calls in backgrounded jobs hang if stdin is still open on tty.
    // So, close it.
    close(0);     // Shut off stdin in case we're in background
    open("/dev/null", O_RDONLY);   // fill it with nullity

    LOG(INFO) << "My own IP address is configured as " << smq.my_ipaddress;
    LOG(INFO) << "The HLR registry is at " << smq.my_register_hostport;

    savefile = gConfig.getStr("savefile");

    if (!smq.read_queue_from_file (savefile)) {
	LOG(WARN) << "Failed to read queue from file " << savefile;
    }
    LOG(INFO) << "Queue contains " << smq.time_sorted_list.size() << " msgs.";

    // smq.debug_dump();

    smq.stop_main_loop = false;
    smq.reexec_smqueue = false;

    smq.main_loop();

    // The rest of this code never gets run (unless main_loop exits
    // based upon getting a "reboot" sms or signal or something).
    if (smq.reexec_smqueue) {
      LOG(WARN) << "====== Re-Execing! ======";
      if (!smq.save_queue_to_file(savefile)) {
	LOG(ERROR) << "OUCH!  Could not save queue to file " << savefile;
      }
      please_re_exec = true;
      break;	// Get out of scope that contains smq, closing file descrs.
    } else {
      LOG(NOTICE) << "====== Quitting! ======";
      if (!smq.save_queue_to_file(savefile)) {
	LOG(ERROR) << "OUCH!  Could not save queue to file " << savefile;
      }
      // smq.debug_dump();
      break;	// don't re-exec the main loop.
    }
  }

  // Free up any OSIP stuff, to make valgrind squeaky clean.
  osip_mem_release();

  if (please_re_exec)
    execvp(argv[0], argv);
  return 0;
}
