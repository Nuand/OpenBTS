/*
 * SMSC.h - SMS Center implementation for OpenBTS.
 * Written by Alexander Chemeris, 2010.
 *
 * Copyright 2010 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * See the COPYING file in the main directory for details.
 */

#include <Regexp.h>
#include <Logger.h>

using namespace std;


#include "smsc.h"

// FORWARD DECLARATIONS
void set_to_for_smsc(const char *address, short_msg_p_list::iterator &smsg);

extern short_code_map_t short_code_map;

/**@name Functions for transmitting through various gateways. */
//@{


/** Substitute spaces with +'s for compatibility with some protocols. */
void convertText(char *dest, const char *src)
{
	// FIXME -- We should just do full "percent encoding" here.
	while (*src != '\0') {
		if (*src == ' ') *dest++ = '+';
		else *dest++ = *src;
		src++;
	}
	*dest = '\0';
}



/** Send SMS via and HTTP interface. */
short_code_action sendHTTP(const char* destination, const std::string &message)
{
	char convMessage[message.length()+2];
	convertText(convMessage,message.data());
	char c1[1024];
	sprintf(c1,"wget -t %d -T %d -q -O - \"%s\"",
		(int)gConfig.getNum("SMS.HTTPGateway.Retries"),
		(int)gConfig.getNum("SMS.HTTPGateway.Timeout"),
		gConfig.getStr("SMS.HTTPGateway.URL").c_str());
	char command[1500];
	// FIXME -- Check specs for a good timeout value here.
	sprintf(command,c1, destination, convMessage);
	LOG(INFO) << "sending via HTTP:" << command;

	// HTTP "GET" method with wget.
	// FIXME -- Look at the output of wget to check success.
	FILE* wget = popen(command,"r");
	if (!wget) {
		LOG(ALERT) << "cannot open wget with " << command;
		return SCA_INTERNAL_ERROR;
	}
	pclose(wget);
	return SCA_DONE;
}


/** Send e-mail with local sendmail program. */
short_code_action sendEMail(const char* address, const char* body, const char* subject=NULL)
{

	// FIXME -- This is broken under Ubuntu because the mail program is different from BSD's.
	LOG(WARNING) << "sending via SMTP is broken under Ubuntu, message body will likely be lost: " << address << ": " << body;

	// We're not checking for overflow because the TPDU can't be more than a few hundred bytes.

	// Build the command line.
	// FIXME -- Use sendmail to have better header control.
	char command[1024];
	if (subject) sprintf(command,"mail -s \"%s\" %s",subject,address);
	else sprintf(command,"mail %s",address);
	LOG(INFO) << "sending via SMTP: \"" << body << "\" via \"" << command << "\"";

	// Send the mail.
	FILE* mail = popen(command,"w");
	if (!mail) {
		LOG(ALERT) << "cannot send mail with \"" << command << "\"";
		return SCA_INTERNAL_ERROR;
	}
	// FIXME -- We should be sure body is 7-bit clean.
	// FIXME -- Ubuntu mail program automatically requests cc:.
	// That breaks portability.  How do we fix that?
	fprintf(mail,"%s",body);
	if (pclose(mail) == -1) return SCA_INTERNAL_ERROR;
	return SCA_DONE;
}

short_code_action sendSIP_init(const char *imsi, const TLSubmit& submit,
                               const std::string &body, short_code_params *scp)
{
	const char *address = submit.DA().digits();
	const TLUserData& tl_ud = submit.UD();
	const char *from = scp->scp_qmsg_it->parsed->from->url->username;
	LOG(INFO) << "from " << imsi << " to " << address;

	if (scp == NULL)
	{
		LOG(WARNING) << "short_code_params is NULL. Error.";
		return SCA_INTERNAL_ERROR;
	}

	// START OF THE SIP PROCESSING
	osip_message_t *omsg = scp->scp_qmsg_it->parsed;

	// Req.URI
	osip_free(omsg->req_uri->username);
	omsg->req_uri->username = (char *)osip_malloc (strlen(address)+1);
	strcpy(omsg->req_uri->username, address);

	// To:
	set_to_for_smsc(address, scp->scp_qmsg_it);

	// Let them know that parsed part has been changed.
	scp->scp_qmsg_it->parsed_was_changed();

	/*if (ISLOGGING(DEBUG)) {
		// Call make_text_valid() is needed for debug only.
		scp->scp_qmsg_it->make_text_valid();
		LOG(DEBUG) << "Updated SMS message: " << scp->scp_qmsg_it->text;
	}*/
	return SCA_RESTART_PROCESSING;
}

short_code_action sendSIP_init_text(const char *imsi,
                                    const std::string &body, short_code_params *scp)
{
	const char *address = scp->scp_qmsg_it->parsed->to->url->username;
	const char *from = scp->scp_qmsg_it->parsed->from->url->username;
	LOG(INFO) << "from " << imsi << " to " << address;

	if (scp == NULL)
	{
		LOG(WARNING) << "short_code_params is NULL. Error.";
		return SCA_INTERNAL_ERROR;
	}

	// START OF THE SIP PROCESSING
	osip_message_t *omsg = scp->scp_qmsg_it->parsed;

	// Req.URI
	osip_free(omsg->req_uri->username);
	omsg->req_uri->username = (char *)osip_malloc (strlen(address)+1);
	strcpy(omsg->req_uri->username, address);

	// To:
	set_to_for_smsc(address, scp->scp_qmsg_it);

	// Let them know that parsed part has been changed.
	scp->scp_qmsg_it->parsed_was_changed();

	/*if (ISLOGGING(DEBUG)) {
		// Call make_text_valid() is needed for debug only.
		scp->scp_qmsg_it->make_text_valid();
		LOG(DEBUG) << "Updated SMS message: " << scp->scp_qmsg_it->text;
	}*/
	return SCA_RESTART_PROCESSING;
}

void pack_tpdu(short_msg_p_list::iterator &smsg)
{
	// Pack RP-DATA to bitstream
	RLFrame RPDU_new(smsg->rp_data->bitsNeeded());
	smsg->rp_data->write(RPDU_new);
	LOG(DEBUG) << "New RLFrame: " << RPDU_new;

	// START OF THE SIP PROCESSING
	osip_message_t *omsg = smsg->parsed;

	// Message body
	osip_body_t *bod1 = (osip_body_t *)omsg->bodies.node->element;
	osip_free(bod1->body);
	ostringstream body_stream;
	RPDU_new.hex(body_stream);
	bod1->length = body_stream.str().length();
	bod1->body = (char *)osip_malloc (bod1->length+1);
	strcpy(bod1->body, body_stream.str().data());

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();
}

void create_sms_delivery(const std::string &body,
			 const TLUserData &UD,
                         short_msg_p_list::iterator &smsg)
{
	RPData *rp_data_new = NULL;
	TLDeliver *deliver = NULL;
	const char *from = smsg->parsed->from->displayname;
	if (from == 0){
	    from = smsg->parsed->from->url->username;
	}

	unsigned TLPID = 0;
	if (smsg->tl_message!=NULL) {
		TLPID = ((TLSubmit*)smsg->tl_message)->PI(); //TP-PID
		if (TLPID != 0x40) TLPID = 0; //just to be safe
	}

	// HACK
	// Check for "Easter Eggs"
	// TL-PID
	// See 03.40 9.2.3.9.
	if (strncmp(body.data(),"#!TLPID",7)==0) sscanf(body.data(),"#!TLPID%d",&TLPID);

	// Generate RP-DATA with SMS-DELIVER
	{
		unsigned reference = random() % 255;
		deliver = new TLDeliver(from,UD,TLPID);
		LOG(DEBUG) << "New TLDeliver: " << *deliver;
		rp_data_new = new RPData(reference, RPAddress(gConfig.getStr("SMS.FakeSrcSMSC").c_str()),
		                         *deliver);
		LOG(DEBUG) << "New RPData: " << *rp_data_new;
	}

	// Replace RP-DATA in the message
	delete smsg->rp_data;
	delete smsg->tl_message;
	smsg->rp_data = rp_data_new;
	smsg->tl_message = deliver;

	// Pack RP-Data to bitstream and update SIP message
	pack_tpdu(smsg);

	// Now we have SC->MS message
	smsg->ms_to_sc = false;

}

void set_to_for_smsc(const char *address, short_msg_p_list::iterator &smsg)
{
	osip_message_t *omsg = smsg->parsed;

	// Set To field
	osip_free(omsg->to->url->username);
	osip_free(omsg->to->displayname);
	omsg->to->url->username = (char *)osip_malloc (strlen(address)+1);
	omsg->to->displayname = (char *)osip_malloc (strlen(omsg->req_uri->username)+1);
	strcpy(omsg->to->url->username, address);
	strcpy(omsg->to->displayname, omsg->req_uri->username);

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();
}

//@}


/** Send a TPDU through whatever gateway is available.  */
short_code_action submitSMS(const char *imsi, const TLSubmit& submit,
                            const std::string &body, short_code_params *scp)
{
	LOG(INFO) << "from " << imsi << " message: " << submit;
	const TLAddress& address = submit.DA();

	// Hey! You can get to the HLR through the scp argument!
	SubscriberRegistry& hlr = scp->scp_smq->my_hlr;

	// Is this message going to a short code?
	// Short codes costs are special cases and will be checked im the short code handlers thmeselves.
	bool isShortCode = (short_code_map.find(address.digits()) != short_code_map.end());

	// TODO: If this is a short code, and we know that the cost will be 0, why waste time hitting the subscriber registry? -DCK
	//       That means, don't look up the service, and don't check to see if any user is prepaid.

	// FIXME: There are some helper functions in smqueue.cpp. Why not use those? Or move them somewhere to be used commonly. -DCK

	// Get the delivery cost.
	string service;
	const char * dialedNumber = address.digits();
	if (hlr.getIMSI(dialedNumber)) service = gConfig.getStr("ServiceType.Local");
	else service = gConfig.getStr("ServiceType.Networked");
	int cost = hlr.serviceCost(service.c_str());
	if (isShortCode) cost = 0;
	if (cost>=0) scp->scp_qmsg_it->cost = cost;
	else { LOG(ALERT) << "cannot get cost for service type " << service; }
	scp->scp_qmsg_it->service = service;

	// If the message is NOT inbound from the relay, bill
	if (!scp->scp_qmsg_it->from_relay) {
		// Check the subscriber's balance now.
		bool prepaid;
        	hlr.isPrepaid(imsi,prepaid);
		int accountBalance = 0;
		SubscriberRegistry::Status stat = hlr.balanceRemaining(imsi,accountBalance);
		if (stat != SubscriberRegistry::SUCCESS) { LOG(ALERT) << "cannot check account for user " << imsi; }
		if (prepaid && cost!=0 && accountBalance<cost) {
			ostringstream os;
			// TODO: Make customizable
			os << "Account balance of " << accountBalance << " too low for service cost of " << cost << ".";
			scp->scp_reply = new_strdup(os.str().c_str());
			return SCA_REPLY;
		}
	}

//#if 0
//This is broken under Unbuntu becasue of changes in the "mail" program.

	// Check for direct e-mail address at start of message body.
	// FIXME -- This doesn't really follow the spec.  See GSM 03.40 3.8.
	static const Regexp emailAddress("^[[:graph:]]+@[[:graph:]]+ ");
	if (emailAddress.match(body.data())) {
		char bodyCopy[body.length()+2];
		strcpy(bodyCopy,body.data());
		char* SMTPAddress = bodyCopy;
		char* term = strchr(bodyCopy,' ');
		// If term's NULL, the regexp emailAddress is broken.
		assert(term);
		*term = '\0';
		char* SMTPPayload = term+1;
		// Get the sender's E.164 to put in the subject line.
		char* clid = hlr.getCLIDLocal(imsi);
		char subjectLine[200];
		if (!clid) sprintf(subjectLine,"from %s",imsi);
		else {
			sprintf(subjectLine,"from %s",clid);
			free(clid);
		}
		// Send it.
		LOG(INFO) << "sending SMTP to " << SMTPAddress << ": " << SMTPPayload;
		if (SMTPPayload) return sendEMail(SMTPAddress,SMTPPayload,subjectLine);
		else return sendEMail(SMTPAddress,"(empty)","from OpenBTS gateway");
	}
//#endif

	char* destinationNumber = scp->scp_smq->my_hlr.getIMSI(address.digits());

	// Send to smqueue or HTTP gateway, depending on what's defined in the config.
	// And whether of not we can resolve the destination, and a global relay does not exist,
	// AND the message is not to a shortcode.
	if (gConfig.getStr("SIP.GlobalRelay.IP").length() == 0 && gConfig.getStr("SMS.HTTPGateway.URL").length() != 0 &&
		!destinationNumber && !isShortCode)
		// If there is an external HTTP gateway, use it.
		return sendHTTP(address.digits(), body);

	free(destinationNumber);
	return sendSIP_init(imsi, submit, body, scp);
}

/*void create_sms_submit(const std::string &body,
			 const TLUserData &UD,
                         short_msg_p_list::iterator &smsg)
{
	RPData *rp_data_new = NULL;
	TLSubmit *submit = NULL;
	const char *from = smsg->parsed->from->url->username;

	// HACK
	// Check for "Easter Eggs"
	// TL-PID
	// See 03.40 9.2.3.9.
	unsigned TLPID=0;
	if (strncmp(body.data(),"#!TLPID",7)==0) sscanf(body.data(),"#!TLPID%d",&TLPID);

	// Generate RP-DATA with SMS-SUBMIT
	{
		unsigned reference = random() % 255;
		submit = new TLSubmit();//from,UD,TLPID);
		rp_data_new = new RPData(reference, RPAddress(gConfig.getStr("SMS.FakeSrcSMSC").c_str()),
		                         *submit);
		LOG(DEBUG) << "New RPData: " << *rp_data_new;
	}

	// Replace RP-DATA in the message
	delete smsg->rp_data;
	delete smsg->tl_message;
	smsg->rp_data = rp_data_new;
	smsg->tl_message = submit;
}

void convert_from_plain(const std::string &body, short_msg_p_list::iterator &smsg)
{
	smsg->parse();

	smsg->content_type = short_msg::VND_3GPP_SMS;
	create_sms_submit(body, TLUserData(body.data()), smsg);

	// Pack RP-DATA to bitstream
	RLFrame RPDU_new(smsg->rp_data->bitsNeeded());
	smsg->rp_data->write(RPDU_new);
	LOG(DEBUG) << "New RLFrame: " << RPDU_new;

	// START OF THE SIP PROCESSING
	osip_message_t *omsg = smsg->parsed;

	// Message body
	osip_body_t *bod1 = (osip_body_t *)omsg->bodies.node->element;
	osip_free(bod1->body);
	ostringstream body_stream;
	RPDU_new.hex(body_stream);
	bod1->length = body_stream.str().length();
	bod1->body = (char *)osip_malloc (bod1->length+1);
	strcpy(bod1->body, body_stream.str().data());

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();
}*/

short_code_action shortcode_smsc(const char *imsi, const char *msgtext,
                                 short_code_params *scp)
{
	short_code_action return_action = SCA_TREAT_AS_ORDINARY;
	short_msg_p_list::iterator smsg = scp->scp_qmsg_it;

	if (smsg->content_type != short_msg::VND_3GPP_SMS)
	{
		if (smsg->content_type == short_msg::TEXT_PLAIN) {
			return sendSIP_init_text(imsi, msgtext, scp);//convert_from_plain(msgtext, smsg);
		} else {
			LOG(WARNING) << "We support only TPDU-coded SMS messages or plain text encoded SMS messages.";
			return SCA_INTERNAL_ERROR;
		}
	}
	if (smsg->tl_message == NULL)
	{
		LOG(WARNING) << "TLMessage is not available. Error during SMS decoding occurred?";
		return SCA_INTERNAL_ERROR;
	}
	if (((TLMessage::MessageType)smsg->tl_message->MTI()) != TLMessage::SUBMIT)
	{
		LOG(WARNING) << "TPDU must be of SMS-SUBMIT type, we have MTI="
		          << (TLMessage::MessageType)smsg->tl_message->MTI();
		return SCA_INTERNAL_ERROR;
	}

	return_action = submitSMS(imsi, *((TLSubmit*)smsg->tl_message), msgtext, scp);

	return return_action;
}

bool pack_text_to_tpdu(const std::string &body,
                       short_msg_p_list::iterator &smsg)
{
	create_sms_delivery(body, TLUserData(body.data()), smsg);

	// Set Content-Type field
	const char *type = "application";
	const char *subtype = "vnd.3gpp.sms";
	osip_free(smsg->parsed->content_type->type);
	osip_free(smsg->parsed->content_type->subtype);
	smsg->parsed->content_type->type = (char *)osip_malloc (strlen(type)+1);
	smsg->parsed->content_type->subtype = (char *)osip_malloc (strlen(subtype)+1);
	strcpy(smsg->parsed->content_type->type, type);
	strcpy(smsg->parsed->content_type->subtype, subtype);

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();

	return true;
}

bool pack_plain_text(const std::string &body,
                       short_msg_p_list::iterator &smsg)
{
	// START OF THE SIP PROCESSING
	osip_message_t *omsg = smsg->parsed;

	// Message body
	osip_body_t *bod1 = (osip_body_t *)omsg->bodies.node->element;
	osip_free(bod1->body);
	bod1->length = body.length();
	bod1->body = (char *)osip_malloc (bod1->length+1);
	strcpy(bod1->body, body.data());

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();

	// It's SC->MS now
	smsg->ms_to_sc = false;

	// Set Content-Type field
	const char *type = "text";
	const char *subtype = "plain";
	osip_free(smsg->parsed->content_type->type);
	osip_free(smsg->parsed->content_type->subtype);
	smsg->parsed->content_type->type = (char *)osip_malloc (strlen(type)+1);
	smsg->parsed->content_type->subtype = (char *)osip_malloc (strlen(subtype)+1);
	strcpy(smsg->parsed->content_type->type, type);
	strcpy(smsg->parsed->content_type->subtype, subtype);

	// Let them know that parsed part has been changed.
	smsg->parsed_was_changed();

	return true;
}

bool recode_tpdu(const std::string &body,
                 short_msg_p_list::iterator &smsg)
{
	bool return_action = true;

	// Safety check
	if (smsg->tl_message == NULL)
	{
		LOG(WARNING) << "TLMessage is not available. Error during SMS decoding occurred?";
		return false;
	}

	switch ((TLMessage::MessageType)smsg->tl_message->MTI()) {
	case TLMessage::SUBMIT: {
		TLSubmit *submit = (TLSubmit*)smsg->tl_message;
		const TLUserData& tl_ud = submit->UD();
		create_sms_delivery(body, tl_ud, smsg);
		return_action = true;
		break;
	}

	case TLMessage::DELIVER_REPORT:
	case TLMessage::STATUS_REPORT:
		// TODO
		LOG(WARNING) << "TPDU must be of SMS-SUBMIT type, we have MTI="
		          << (TLMessage::MessageType)smsg->tl_message->MTI();
		return false;
	}

	return return_action;
}

bool pack_sms_for_delivery(short_msg_p_list::iterator &smsg)
{
	bool return_action = true;
	std::string msgtext;

	if (!smsg->need_repack) {
		// Nothing to do.
		return true;
	}

	// Parse message if not parsed yet.
	smsg->parse();

	// Get message text in plain text form
	msgtext = smsg->get_text();

	short_msg::ContentType content_type = smsg->content_type;


	// Convert the message if we are requested to.
	if (smsg->convert_content_type != short_msg::UNSUPPORTED_CONTENT) {
		LOG(DEBUG) << "Converting message to " << smsg->convert_content_type;
		switch (smsg->convert_content_type) {
		case short_msg::TEXT_PLAIN:
			return_action = pack_plain_text(msgtext, smsg);
			break;

		case short_msg::VND_3GPP_SMS:
			// TODO: Test and verify
			return_action = pack_text_to_tpdu(msgtext, smsg);
			break;

		case short_msg::UNSUPPORTED_CONTENT:
		default:
			LOG(WARNING) << "Unsupported SIP-messages content.";
			return false;
		}
	} else {
		switch (content_type) {
		case short_msg::TEXT_PLAIN:
			return_action = pack_text_to_tpdu(msgtext, smsg);
			break;

		case short_msg::VND_3GPP_SMS:
			// No need to recode if it's SC->MS already
			if (smsg->ms_to_sc)
			{
				return_action = recode_tpdu(msgtext, smsg);
			}
			break;

		case short_msg::UNSUPPORTED_CONTENT:
		default:
			LOG(WARNING) << "Unsupported SIP-messages content.";
			return false;
		}
	}

	// Successful repack
	if (return_action) {
		smsg->need_repack = false;
	}

	/*if (ISLOGGING(DEBUG)) {
		// Call make_text_valid() is needed for debug only.
		smsg->make_text_valid();
		LOG(DEBUG) << "Updated SMS message: " << smsg->text;
	}*/

	return return_action;
}
