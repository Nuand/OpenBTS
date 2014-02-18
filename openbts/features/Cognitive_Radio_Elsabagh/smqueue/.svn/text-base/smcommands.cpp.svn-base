/*
 * Smcommands.cpp - Short Message (SMS) commands ("shortcodes") for OpenBTS.
 * Written by John Gilmore, August 2009.
 *
 * Configuration table added by David A. Burgess, January 2010.
 *
 * Copyright 2009, 2010 Free Software Foundation, Inc.
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
#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>			// strtol
#include <time.h>			// ctime_r

#include <Configuration.h>

using namespace std;
using namespace SMqueue;

extern ConfigurationTable gConfig;


/* Seekrit SMS message to make the server quit for valgrind leak checking. */
enum short_code_action
whiplash_quit (const char *imsi, const char *msgtext, short_code_params *scp)
{
	const char* psswd = gConfig.getStr("SC.WhiplashQuit.Password");
	unsigned len = strlen(psswd);
	
	/* Messages to this code are special commands, perhaps */
	if (!strncmp(psswd, msgtext, len)) {
		if (!strncmp("quit", msgtext+len+1, 4)) {
			return SCA_QUIT_SMQUEUE;
		}
		if (!strncmp("exec", msgtext+len+1, 4)) {
			return SCA_EXEC_SMQUEUE;
		}
		if (!strncmp("testsave", msgtext+len+1, 4)) {
			scp->scp_smq->save_queue_to_file(gConfig.getStr("SC.WhiplashQuit.SaveFile"));
			scp->scp_reply = new_strdup("Done.");
			return SCA_REPLY;
		}
		scp->scp_reply = new_strdup("Unknown Command");
		return SCA_REPLY;
	}	
	return SCA_TREAT_AS_ORDINARY;
}

enum short_code_action
shortcode_debug_dump (const char *imsi, const char *msgtext,
		      short_code_params *scp)
{
	scp->scp_smq->debug_dump();
	return SCA_DONE;
}

enum short_code_action
shortcode_quick_chk (const char *imsi, const char *msgtext,
		      short_code_params *scp)
{
	ostringstream answer;
	
	answer << scp->scp_smq->time_sorted_list.size() << " queued.";
	scp->scp_reply = new_strdup(answer.str().c_str());
	return SCA_REPLY;
}

/*
 * 411 -- information.
 * Start by telling people their phone number.
 */
enum short_code_action
shortcode_four_one_one (const char *imsi, const char *msgtext,
		      short_code_params *scp)
{
	ostringstream answer;
	
        short_msg_p_list::iterator x;
        int n = 0, missing = 0, registering = 0, bouncing = 0;
        
        for (x = scp->scp_smq->time_sorted_list.begin();
             x != scp->scp_smq->time_sorted_list.end(); x++) {
	    n++;
	    switch (x->state) {
		case REQUEST_DESTINATION_SIPURL:
		case REQUEST_MSG_DELIVERY:
		case ASKED_FOR_MSG_DELIVERY:
		case AWAITING_TRY_MSG_DELIVERY:
		    x->make_text_valid();
		    if (0 == strcmp ("127.0.0.1", x->parsed->req_uri->host))
			missing++;
		    if (0 == strcmp (gConfig.getStr("SC.Register.Code"),x->parsed->from->url->username))
			registering++;
		    if (0 == strcmp (gConfig.getStr("SC.Register.Code"), x->parsed->req_uri->username))
		    	registering++;
		    if (0 == strcmp (gConfig.getStr("SC.Info.Code"), x->parsed->from->url->username))
			bouncing++;
		    break;

		case AWAITING_REGISTER_HANDSET:
		case REGISTER_HANDSET:
		case ASKED_TO_REGISTER_HANDSET:
		    registering++;
		    break;

		default: 
		    ;
            }
        }
        answer <<  n << " queued";
	if (missing)
	    answer << ", " << missing << " unlocatable";
	if (registering)
	    answer << ", " << registering << " registering";
	if (bouncing)
	    answer << ", " << bouncing << " bouncing";

	answer << ", ";
	const char *mycell = scp->scp_smq->my_network.string_addr (
			    (sockaddr *)scp->scp_qmsg_it->srcaddr, 
			    scp->scp_qmsg_it->srcaddrlen, false).c_str();
	mycell += strlen(mycell) - 3;	// Hack to only show last 3 digits
	answer << "cell " << mycell;

	answer << ", ";
	char *username = scp->scp_qmsg_it->parsed->from->url->username;
	answer << username;

	answer << ", ";
	char *newfrom = scp->scp_smq->my_hlr.getCLIDLocal(username);
	answer << "phonenum " << newfrom;

	time_t now = time(NULL);
	char timebuf[26+/*slop*/4];	// 
	answer << ", ";
	ctime_r(&now, timebuf);
	timebuf[19] = '\0';	// Leave out space, year and newline
	answer << "at " << timebuf+4;	// Leave out day of week

	answer << ", ";
	answer << "'" << msgtext << "'";

	scp->scp_reply = new_strdup(answer.str().c_str());
	return SCA_REPLY;
}

/*
 * Remove a message from the queue, by its tag.
 * If first char is "-", don't reply, just do it.
 * (this keeps the reply out of the queue, while debugging.)
 * If argument is "6000", then delete any queued message with
 * timeout greater than 5000 seconds.
 */
enum short_code_action
shortcode_zap_queued (const char *imsi, const char *msgtext,
		      short_code_params *scp)
{
	ostringstream answer;
    short_msg_p_list::iterator sent_msg;
    short_msg_p_list resplist;
    bool noreply = false;

    if (msgtext[0] == '-') {
        noreply = true;
        msgtext++;
    }

    if (!strcmp(gConfig.getStr("SC.ZapQueued.Password"), msgtext)) {
        // Delete all messages in queue in NO_STATE state or with
        // huge timeouts.
        short_msg_p_list::iterator x;
        time_t toolate = 5000 
                       + scp->scp_smq->time_sorted_list.begin()->gettime();
        int n = 0;
        
        for (x = scp->scp_smq->time_sorted_list.begin();
             x != scp->scp_smq->time_sorted_list.end(); x++) {
            if (x->state == NO_STATE || toolate <= x->next_action_time) {
                n++;
                resplist.splice(resplist.begin(),
                                scp->scp_smq->time_sorted_list, x);
                resplist.pop_front();   // pop and delete the sent_msg.
            }
        }
        answer <<  "Removed " << n << " messages.";
    } else {
        // figure out what message we're deleting, by tag
        if (!scp->scp_smq->find_queued_msg_by_tag(sent_msg, msgtext)) {
                // No message in queue.
                answer << "No message queued with tag '" << msgtext
                       << "'.";
        } else {
            answer << "Deleting queued msg '" << msgtext 
                   << " in state " << sent_msg->state
                   << " and timeout " 
                   << sent_msg->next_action_time - sent_msg->gettime();
           resplist.splice(resplist.begin(),
                           scp->scp_smq->time_sorted_list, sent_msg);
           resplist.pop_front();   // pop and delete the sent_msg.
        }
    }

    scp->scp_reply = new_strdup(answer.str().c_str());
    return noreply? SCA_DONE: SCA_REPLY;
}

/*
 * Register the user's phone with the HLR.
 *
 * This is how new phones get into the system!
 * The message includes the user's phone number, and possible arguments
 * afterward.
 *
 * This shortcode message can be processed several times, if we need
 * to do retries because of locked files, for example.
 */
enum short_code_action
shortcode_register (const char *imsi, const char *msgtext,
		      short_code_params *scp)
{
	SMq *smq = scp->scp_smq;
	ostringstream answer;
	char *existing, *phonenum = NULL, *q;
	const char *p;
	int seendig;
	bool seenplus;
	bool badnum;
	int exclaim;
	HLR::Status did;

	phonenum = new char[strlen(msgtext)+1];
	// FIXME, error-check this sucker very well!
	// Simple parse.
	q = phonenum;
	exclaim = 0;
	seendig = 0;
	seenplus = false;
	badnum = false;
	for (p = msgtext; *p != 0 && !badnum; p++) {
		switch (*p) {
		case '0': case '1': case '2': case '3': case '4': 
		case '5': case '6': case '7': case '8': case '9':
			*q++ = *p;
			seendig++;
			break;
		case '+':
			if (!seendig && !seenplus) {
				*q++ = *p;
				seenplus++;
			} else {
				answer << "Error: + can only be the first character in a phone number";
				badnum++;
			}
			break;
		case ' ': case '(': case ')': case '\r': case '\n':
			break;
		case '!':
			exclaim++;
			break;
		default:
			answer << "Error: invalid character '" << *p << "' in requested number";
			badnum++;
		}
	}
	*q++ = '\0';		// Null-terminate it.
   if (!badnum) {
	bool override = gConfig.defines("SC.Register.Digits.Override") && (exclaim==3);
	if (!override) {
		if ((seendig > gConfig.getNum("SC.Register.Digits.Max")) ||
		    (seendig < gConfig.getNum("SC.Register.Digits.Min"))) {
			answer << phonenum << " not valid with " << seendig << " digits. "
				<< "Must be " << gConfig.getStr("SC.Register.Digits.Min")
				<< " to " << gConfig.getStr("SC.Register.Digits.Max") << " digits.";
			badnum++;
		}
	}
   }

   if (!badnum) {

	existing = smq->my_hlr.getCLIDLocal(imsi);
	if (existing) {
		// There are two ways to get here.  One is to send a
		// registration shortcode when you've already registered.
		// The other is that we were re-called after final
		// registration of the handset.  Figure which.
		if (0 == strcmp(existing, phonenum)) {
			answer << gConfig.getStr("SC.Register.Msg.WelcomeA")
			       << ' ' << phonenum << ' '
			       << gConfig.getStr("SC.Register.Msg.WelcomeB");
		} else {
			answer << gConfig.getStr("SC.Register.Msg.AlreadyA")
			       << ' ' << existing << ' '
			       << gConfig.getStr("SC.Register.Msg.AlreadyB"); 
		}
	} else {
		existing = smq->my_hlr.getIMSI(phonenum);
		if (existing) {
			LOG(DEBUG) << phonenum << " is already in the HLR";
			answer << gConfig.getStr("SC.Register.Msg.TakenA")
			       << ' ' << phonenum << ' '
			       << gConfig.getStr("SC.Register.Msg.TAkenB");
		} else {
			LOG(DEBUG) << phonenum << " is not in the HLR";
			// Neither the IMSI nor the phonenum is in use.
			// Book 'em, danno!
			did = smq->my_hlr.addUser(imsi, phonenum);
			switch (did) {
			case HLR::SUCCESS:
				// Phone#<->IMSI is set up; now register
				// the IMSI<->cell IP:port mapping.
				delete [] phonenum;
				return SCA_REGISTER;

			case HLR::DELAYED:	// FIXME, force reload later.
				delete [] phonenum;
				return SCA_AWAIT_REGISTER;

			case HLR::FAILURE:
			default:
				answer << gConfig.getStr("SC.Register.Msg.ErrorA")
				       << ' ' << phonenum << ' '
				       << gConfig.getStr("SC.Register.Msg.ErrorB")
				       << imsi << '.';
				break;

			case HLR::TRYAGAIN:
				// Locked database, have to retry soon.
				scp->scp_delay = 10;	// seconds
				delete [] phonenum;
				return SCA_RETRY_AFTER_DELAY;
			}
		}
	}
	
   }

	delete [] phonenum;
	LOG(DEBUG) << "answering \"" << answer.str() << "\"";
	scp->scp_reply = new_strdup(answer.str().c_str());
	return SCA_REPLY;
}



/*
 * Here is where we list all the functions that we care to make
 * available -- along with their phone numbers.
 */
void
SMqueue::init_smcommands (short_code_map_t *scm)
{
	if (gConfig.defines("SC.Register.Code"))
		(*scm)[gConfig.getStr("SC.Register.Code")] = shortcode_register;
	if (gConfig.defines("SC.Info.Code"))
		(*scm)[gConfig.getStr("SC.Info.Code")] = shortcode_four_one_one;
	if (gConfig.defines("SC.DebugDump.Code"))
		(*scm)[gConfig.getStr("SC.DebugDump.Code")] = shortcode_debug_dump;
	if (gConfig.defines("SC.QuickChk.Code"))
		(*scm)[gConfig.getStr("SC.QuickChk.Code")] = shortcode_quick_chk;
	if (gConfig.defines("SC.ZapQueued.Code"))
		(*scm)[gConfig.getStr("SC.ZapQueued.Code")] = shortcode_zap_queued;
	if (gConfig.defines("SC.WhiplashQuit.Code"))
		(*scm)[gConfig.getStr("SC.WhiplashQuit.Code")] = whiplash_quit;
//	(*scm)["666"]    = shortcode_text_access;
}
