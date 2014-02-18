/*
 * SMqueue.h - In-memory queue manager for Short Messages (SMS's) for OpenBTS.
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

#ifndef SM_QUEUE_H
#define SM_QUEUE_H

#include <time.h>
#include <osipparser2/osip_message.h>	/* from osipparser2 */
#include <stdlib.h>			/* for osipparser2 */
#include <sys/time.h>			/* for osip_init */
#include <osip2/osip.h>			/* for osip_init */
#include <list>
#include <map>
#include <string>
#include <iostream>

#include "smnet.h"			// My network support
#include "HLR.h"			// My home location register

namespace SMqueue {

/* Maximum text size of an SMS message.  */
#define SMS_MESSAGE_MAX_LENGTH  140

/* std::abort isn't always there.  Neither is the C library version.
   Idiots?  You tell me.  */
void abfuckingort();			// Where is the real one?

/* strdup uses malloc, which doesn't play well with new/delete.
   The idiots who defined C++ don't provide one, so we will. */
char *new_strdup(const char *orig);

/*
 * States that a message can be in, while processed.
 * Messages come in, they need various lookups, then they go out.
 * Then they sit pending acknowledgement that they were successful.
 * The state machine is driven by incoming packets or information,
 * and in the absence of such incoming info, by timeouts.
 */

enum sm_state {				// timeout, next-state-if-timeout
	NO_STATE = 0,
	REQUEST_FROM_ADDRESS_LOOKUP = 1,
	ASKED_FOR_FROM_ADDRESS_LOOKUP = 2,

	AWAITING_TRY_DESTINATION_IMSI = 3,
	REQUEST_DESTINATION_IMSI = 4,
	ASKED_FOR_DESTINATION_IMSI = 5,

	AWAITING_TRY_DESTINATION_SIPURL = 6,
	REQUEST_DESTINATION_SIPURL = 7,
	ASKED_FOR_DESTINATION_SIPURL = 8,

	AWAITING_TRY_MSG_DELIVERY = 9,
	REQUEST_MSG_DELIVERY = 10,
	ASKED_FOR_MSG_DELIVERY = 11,

	DELETE_ME_STATE = 12,

	AWAITING_REGISTER_HANDSET = 13,
	REGISTER_HANDSET = 14,
	ASKED_TO_REGISTER_HANDSET = 15,
	
	STATE_MAX_PLUS_ONE,		/* Keep this one last! */
};

#define STATE_MAX  (STATE_MAX_PLUS_ONE - 1)
#define	INITIAL_STATE	REQUEST_FROM_ADDRESS_LOOKUP

// How to print a state
extern std::string sm_state_strings[STATE_MAX_PLUS_ONE];
std::string sm_state_name(enum sm_state astate);

/* Set this once we've called the initializer for the OSIP parser library. */
extern bool osip_initialized;
extern struct osip *osipptr;

/* In-memory object representing a Short Message.  These are kept as
   text strings (as we received them) and only parsed when we need to
   process them.  This keeps memory usage way down for medium to long
   term storage in the queue.  */
class short_msg {
  public:
	/* First just the text string.   A SIP message including body. */
	unsigned short text_length;
	char *text /* [text_length] */;  // C++ doesn't make it simple

	/* Now a flag for whether it's been parsed, and a parsed copy. */
	bool parsed_is_valid;
	/* If the parsed message has been modified, such that the string
	   copy is no longer valid, this will be true.  */
	bool parsed_is_better;
	osip_message_t *parsed;
	// from;
	// to;
	// time_t date;
	// expiration;

	short_msg () :
		text_length (0),
		text (NULL),
		parsed_is_valid (false),
		parsed_is_better (false),
		parsed (NULL) {
	}
	// Make a short message, perhaps taking responsibility for deleting
	// the "new"-allocated memory passed in.
  	short_msg (int len, char * const cstr, bool use_my_memory) :
		text_length (len),
		text (cstr),
		parsed_is_valid (false),
		parsed_is_better (false),
		parsed (NULL)
	{
		if (!use_my_memory) {
			text = new char [text_length+1];
			strncpy(text, cstr, text_length);
			text[text_length] = '\0';
		}
	};

	/* When created from another short_msg, must copy the string. */
	// We would've liked to make this private, so that we can check at
	// compile time that nobody's inadvertently making copies (malloc's)
	// of short_msg classes -- but unfortunately G++'s <list> doesn't
	// allow lists of classes that have no copy-constructors!
	//private:
  	short_msg (const short_msg &sm) :
		text_length (sm.text_length),
		text (0),
		parsed_is_valid (false),
		parsed_is_better (false),
		parsed (NULL)
	{
		if (text_length) {
			text = new char [text_length+1];
			strncpy(text, sm.text, text_length);
			text[text_length] = '\0';
		}
	};
	public:
#if 0
	short_msg (std::string str) :
		text_length (str.length()),
		text (0),
		parsed_is_valid (false),
		parsed_is_better (false),
		parsed (NULL)
	{
		text = new char [text_length+1];
		strncpy(text, str.data(), text_length);
		text[text_length] = '\0';
	};
#endif
	
	/* Override operator= to avoid pointer-sharing problems */
	private:
	short_msg & operator= (const short_msg &rvalue);
	public:
/*
	{
		this->text_length = rvalue.text_length;
		this->text = new char [rvalue.text_length+1];
		strncpy(this->text, rvalue.text, rvalue.text_length);
		this->text[rvalue.text_length] = '\0';
		this->parsed_is_valid = rvalue.parsed_is_valid;
		this->parsed_is_better = rvalue.parsed_is_better;
		osip_message_clone (rvalue.parsed, &this->parsed);
		return *this;
	} */

	/* Destructor */
	virtual ~short_msg () { unparse(); delete [] text; };

	// Pseudo-constructor due to inability to run constructors on
	// members of lists.
	// Initialize a newly-default-constructed short message,
	// perhaps taking responsibility for deleting
	// the "new"-allocated memory passed in.
	void
	initialize ()
	{	// Nothing to do: default constructor + default initializer
	}

	void
  	initialize (int len, char * const cstr, bool use_my_memory)
	{
		// default constructor needs these things revised to
		// initialize with a message in a string.
		text_length = len;
		text = cstr;
		if (!use_my_memory) {
			text = new char [text_length+1];
			strncpy(text, cstr, text_length);
			text[text_length] = '\0';
		}
	}

	/* Parsing, validating, and unparsing messages.  */
	bool parse() {
		int i;
		osip_message_t *sip;

		if (parsed_is_valid)
			return true;

		if (!osip_initialized) {
			i = osip_init(&osipptr);
			if (i != 0) return false;
			osip_initialized = true;
		}
			
		unparse();	// Free any previous one.

		i = osip_message_init(&sip);
		if (i != 0) abfuckingort();	/* throw out-of-memory */
		i = osip_message_parse(sip, text, text_length);
		if (i != 0) return false;
		parsed = sip;
		parsed_is_valid = true;
		parsed_is_better = false;
		return true;
	}

	/* Anytime a caller CHANGES the values in the parsed tree of the
	   message, they MUST call this, to let the cacheing system
	   know that the cached copy of the text-string message is no
	   longer valid.  Actually, we have TWO such cached copies!  FIXME
	   so we have to invalidate both of them. */
	void
	parsed_was_changed() {
		parsed_is_better = true;
		osip_message_force_update(parsed);   // Tell osip library too
	}

	/* Make the text string valid, if the parsed copy is better.
	   (It gets "better" by being modified, and parsed_was_changed()
	   got called, but we deferred fixing up the text string til now.) */
	void
	make_text_valid() {
		if (parsed_is_better) {
			/* Make or remake text string from parsed version. */
			char *dest = NULL;
			size_t length = 0;

			if (!parsed_is_valid) abfuckingort();
			int i = osip_message_to_str(parsed, &dest, &length);
			if (i != 0) {
				std::cerr << "Parsed is better, can't deal"
					  << std::endl;
				abfuckingort();
			}
			delete [] text;
			/* Because "osip_free" != "delete", we have to recopy
			   the string!!!  Don't you love C++?  */
			text_length = length;
			text = new char [text_length+1];
			strncpy(text, dest, text_length);
			text[text_length] = '\0';
			osip_free(dest);
			parsed_is_valid = true;
			parsed_is_better = false;
		}
		if (text == NULL)
			abfuckingort();
	}

	/* Free up all memory used by parsed version of message. */
	void unparse() {
		if (parsed_is_better)
			make_text_valid();
		if (parsed)
			osip_message_free(parsed);
		parsed = NULL;
		parsed_is_valid = false;
		parsed_is_better = false;
	}

};

// I couldn't figure out how to make these static members of the class...
/* Timeouts when going from NO_STATE into each subsequent state */
//    /*static*/ int timeouts_NO_STATE[STATE_MAX_PLUS_ONE];

/* Index to all timeouts */
extern /*static*/ int (*timeouts[STATE_MAX_PLUS_ONE])[STATE_MAX_PLUS_ONE];

class short_msg_pending: public short_msg {
	public:
	enum sm_state state;		// State of processing
	time_t next_action_time;	// When to do something different
	int retries;			// How many times we've retried
					// this message.
	char srcaddr[16];		// Source address (ipv4 or 6 or ...)
	socklen_t srcaddrlen;		// Valid length of src address.
	char *qtag;			// Tag that identifies this msg
					// uniquely in the queue.
					// (It is set 1st time msg is parsed.)
	int qtaghash;			// Simple hash of the qtag.
	char *linktag;			// Tag of a message that this message
					// is related to.  (We use this in
					// handset register messages, to find
					// the original SMS message that
					// prompted us to send the register.)

	static const char *smp_my_ipaddress;	// Static copy of my IP address
					// for validity checking of msgs.
					// (We get our own copy because
					// it's the only thing we need to
					// inherit from SMq.)
	static const char *smp_my_2nd_ipaddress; // Idiocy for NAT

	/* Constructors */
	short_msg_pending () :
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{ 
	}

	// Make a pending short message, perhaps taking responsibility for 
	// deleting the "new"-allocated memory passed in.
  	short_msg_pending (int len, char * const cstr, bool use_my_memory)
	    : short_msg (len, cstr, use_my_memory),
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{
	}

#if 0
	short_msg_pending (std::string str) : 
		short_msg (str),
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{
	}

	short_msg_pending (const short_msg &sm) :
		short_msg (sm),
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{
	}
#endif

	// 
	// We would've liked to declare this next function PRIVATE,
	// since that would prevent anyone from carelessly duplicating
	// short_msg_pending's in constructors.
	// Unfortunately, libstdc++'s <list> doesn't allow lists to be
	// created from classes that lack a copy constructor.
	//private:
	short_msg_pending (const short_msg_pending &smp) :
		short_msg (static_cast<const short_msg &>(smp)),
		state (smp.state),
		next_action_time (smp.next_action_time),
		retries (smp.retries),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(smp.srcaddrlen),
		qtag (NULL),
		qtaghash (smp.qtaghash),
		linktag (NULL)
	{
		if (smp.srcaddrlen) {
			if (smp.srcaddrlen > sizeof (srcaddr))
				abfuckingort();
			memcpy(srcaddr, smp.srcaddr, smp.srcaddrlen);
		}
			
		if (smp.qtag) {
			int len = strlen(smp.qtag);
			this->qtag = new char[len+1];
			strncpy(this->qtag, smp.qtag, len);
			this->qtag[len] = '\0';
		}
		
		if (smp.linktag) {
			int len = strlen(smp.linktag);
			this->linktag = new char[len+1];
			strncpy(this->linktag, smp.linktag, len);
			this->linktag[len] = '\0';
		}
	}
	public:

	/* Override operator= to avoid pointer-sharing problems */
	private:
	short_msg_pending & operator= (const short_msg_pending &rvalue);
/* FIXME, remove if not needed?
	{
		this->state = rvalue.state;
		this->next_action_time = rvalue.next_action_time;
		this->retries = rvalue.retries;
		this->qtag = rvalue.qtag;
		if (rvalue.srcaddrlen) {
			if (rvalue.srcaddrlen > sizeof (srcaddr))
				abfuckingort();
			memcpy(this->srcaddr, rvalue.srcaddr, rvalue.srcaddrlen);
		}

		if (rvalue.qtag) {
			int len = strlen(rvalue.qtag);
			this->qtag = new char[len+1];
			strncpy(this->qtag, rvalue.qtag, len);
			this->qtag[len] = '\0';
		}
		this->qtaghash = rvalue.qtaghash;
		if (rvalue.linktag) {
			int len = strlen(rvalue.linktag);
			this->linktag = new char[len+1];
			strncpy(this->linktag, rvalue.linktag, len);
			this->linktag[len] = '\0';
		}
	} */
	public:

	/* Destructor */
	virtual ~short_msg_pending () {
		delete [] qtag;
		delete [] linktag;
	}

	/* Methods */

	/* 
	 * Most of the time we can't use a constructor, because we
	 * want to create our short_msg_pending and put it in a list, without
	 * copying it (which would involve lots of useless new/delete's,
	 * particularly if we've parsed it into a huge nested struct).
	 * So we usually make a list with one element (which uses the
	 * default constructor) and then run initialize() on that element
	 * with the same arguments we would've used for the constructor.
	 * This 'constructs' the new short_msg_pending in a temporary list,
	 * and we can then trivially move it into the real message queue,
	 * removing it from the temporary list in the process.
	 *
	 * The wriggling guts under C++ are a little too visible for my taste.
	 */
	void
	initguts ()
	{ 
		state = NO_STATE;
		next_action_time = 0;
		retries = 0;
		memset (srcaddr, 0, sizeof(srcaddr));
		srcaddrlen = 0;
		qtag = NULL;
		qtaghash = 0;
		linktag = NULL;
	}
	void
	initialize ()
	{ 
		short_msg::initialize ();
		// initguts();
	}

	// Make a pending short message, perhaps taking responsibility for 
	// deleting the "new"-allocated memory passed in.
	void
  	initialize (int len, char * const cstr, bool use_my_memory)
	{
		short_msg::initialize (len, cstr, use_my_memory);
		// initguts();
	}

#if 0
	short_msg_pending (std::string str) : 
		short_msg (str),
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{
	}

	short_msg_pending (const short_msg &sm) :
		short_msg (sm),
		state (NO_STATE),
		next_action_time (0),
		retries (0),
		// srcaddr({0}),  // can't seem to initialize an array?
		srcaddrlen(0),
		qtag (NULL),
		qtaghash (0),
		linktag (NULL)
	{
	}
#endif

	/* Optimize this later so we don't make so many kernel calls. */
	time_t gettime () { return time(NULL); };

	/* Reset the message's state and timeout.  Timeout is set based
	   on the current state and the new state.  */
	void set_state(enum sm_state newstate) {
		next_action_time = gettime() +
			(*SMqueue::timeouts[state])[newstate];
		state = newstate;
		/* If we're in a queue, some code in another class is now going
		   to have to change our queue position.  */
	};

	/* Reset the message's state and timeout.  Timeout is argument.  */
	void set_state(enum sm_state newstate, time_t timeout) {
		next_action_time = timeout;
		state = newstate;
		/* If we're in a queue, some code in another class is now going
		   to have to change our queue position.  */
	};

	/* Check that the message is valid, and set the qtag and qtaghash
	   from the message's contents.   Result is 0 for valid, or
	   SIP response error code (e.g. 405).  */
	int validate_short_msg();

	// Set the qtag and qtaghash from the parsed fields.
	// Whenever we change any of these fields, we have to recalculate
	// the qtag.  
	// FIXME, we assume that the CSEQ, Call-ID, and From tag all are
	// components that, in combination, identify the message uniquely.
	// FIXME!  The spec is unpleasantly unclear about this.
	// Result is 0 for success, or 3-digit integer error code if error.
	int set_qtag();

	// Hash the tag to an int, for speedier searching.
	int taghash_of(const char *tag);

	/* Check host and port for validity.  */
	bool
	check_host_port(char *host, char *port);
};

typedef std::list<short_msg_pending> short_msg_p_list;

/*
 * Function parameters and return value for short-code "command" functions that
 * process SMS messages internally rather than sending the SMS message
 * on to somebody else.
 */
class SMq;

enum short_code_action {
	SCA_DONE = 0,
	SCA_INTERNAL_ERROR = 1,
	SCA_REPLY = 2,
	SCA_RETRY_AFTER_DELAY = 3,
	SCA_REPLY_AND_RETRY = 4,
	SCA_QUIT_SMQUEUE = 5,
	SCA_AWAIT_REGISTER = 6,
	SCA_REGISTER = 7,
	SCA_TREAT_AS_ORDINARY = 8,
	SCA_EXEC_SMQUEUE = 9
};

class short_code_params {
  public:
	int scp_retries;		// in: 0 if first call for this msg
	SMq *scp_smq;			// in: The entire "global" SMq structure
	short_msg_p_list::iterator scp_qmsg_it; // in: iterator for our msg 
	char *scp_reply;		// out: Reply msg to sender
	int scp_delay;			// out: Delay before re-call

	short_code_params() :
		scp_retries(0), scp_smq (NULL), scp_qmsg_it (),
		scp_reply (NULL), scp_delay (0) 
	{ }
  private:
	// Avoid copy-construction and assignment by making private.
	short_code_params(const short_code_params &) :
		scp_retries(0), scp_smq (NULL), scp_qmsg_it (),
		scp_reply (NULL), scp_delay (0) 
	{ }
	short_code_params & operator=(const short_code_params &);
  public:

	~short_code_params() {
		delete [] scp_reply;
	}
};

// Function pointer declaration for short-code action functions
typedef enum short_code_action (*short_func_t)
			(const char *imsi, const char *msgtext, 
			 short_code_params *scp);

/*
 * Associative map between target phone numbers (short codes) and
 * function pointers that implement those numbers.
 */
typedef std::map<std::string,short_func_t> short_code_map_t;

/* What fills in that map */
void init_smcommands (short_code_map_t *scm);

/* 
 * Main class for SIP Short Message processing.
 * The daemon is designed to be running one copy of this class.
 */
class SMq {
	public:

	/* A list of all messages we know about, sorted by time of next
	   action (assuming nothing arrives to change our mind before that
	   time). */
	short_msg_p_list time_sorted_list;

	/* We may later want other accessors for faster access to various
	   messages when things DO arrive.  For now, linear search!  */

	/* The network sockets that we're using for I/O */
	SMnet my_network;

	/* The interface to the Host Location Register for routing
	   messages and looking up their return and destination addresses.  */
	AsteriskHLR my_hlr;

	/* Where to send SMS's that we can't route locally. */
	std::string global_relay;

	/* My IP address (I can't tell how I look to others). */
	std::string my_ipaddress;
	/* Idiocy for NAT */
	std::string my_2nd_ipaddress;

	/* My port number. */
	std::string my_udp_port;

	/* The IP addr:port of the HLR, where we send SIP REGISTER
	   messages to associate IMSIs with cell site addr:port numbers. */
	std::string my_register_hostport;

	/* The call-ID, CSeq, and flag we use in registration requests */
	std::string register_call_id;
	int register_call_seq;
	bool have_register_call_id;

	/* Set this to true when you want main loop to stop.  */
	bool stop_main_loop;
	/* Set this to true when you want the program to re-exec itself
	   instead of terminating after the main loop stops.  */
	bool reexec_smqueue;

	/* Constructor */
	SMq () : 
		time_sorted_list (),
		my_network (),
		my_hlr(),
		global_relay(""),
		my_ipaddress(""),
		my_2nd_ipaddress(""),
		my_udp_port(""),
		my_register_hostport(""),
		register_call_id(""),
		register_call_seq(0),
		have_register_call_id(false),
		stop_main_loop (false),
		reexec_smqueue (false)
	{
	}

	// Override operator= so -Weffc++ doesn't complain
	// *DISABLE* assignments by making the = operation private.
	private:
	SMq & operator= (const SMq &rvalue);
	public:

	/* Destructor */

	/* Set my own IP address, since I can't tell how I look to others. */
	void set_my_ipaddress(std::string myip) {
		my_ipaddress = myip;
		// Point to it for message validity checking.
		// NOTE: that copy shares same storage as this one.
		short_msg_pending::smp_my_ipaddress = myip.c_str();
	}

	/* Set my 2nd IP address, since NAT is widespread among idiots. */
	void set_my_2nd_ipaddress(std::string myip) {
		my_2nd_ipaddress = myip;
		// Point to it for message validity checking.
		// NOTE: that copy shares same storage as this one.
		short_msg_pending::smp_my_2nd_ipaddress = myip.c_str();
	}

	/* Set the global relay address (host:port string) */
	void set_global_relay(std::string gr) {
		global_relay = gr;
	}

	/* Set the register host & port -- where to register handsets */
	void set_register_hostport(std::string hp) {
		my_register_hostport = hp;
	}

	/* Initialize the listener -- sets up and opens my_network. */
	bool init_listener (std::string port) {
		my_udp_port = port;
		return my_network.listen_on_port (port);
	}

	// Main loop listening for dgrams and processing them.
	void main_loop();

	/* If nothing happens for a while, handle that.  */
	void process_timeout();

	/* Send a SIP response to acknowledge reciept of a short msg. */
	void respond_sip_ack(int errcode, short_msg_pending *smp, 
		char *netaddr, size_t netaddrlen);

	/*
	 * Originate a short message
	 * Put it in the queue and start handling it.
	 * From is a shortcode (currently),
	 * To is an IMSI (currently),
	 * msgtext is plain ASCII text.
	 * Result is 0 for success, negative for error.
	 */
	int
	originate_sm(const char *from, const char *to, const char *msgtext,
			enum sm_state firststate);

	/*
	 * Originate half of a short message
	 * Put it in the queue and start handling it, but don't actually
	 * finish it or send it; return it to the caller for further mucking.
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
	originate_half_sm(std::string method);

	/*
	 * Send a bounce message, based on an existing queued message.
	 * Return the state to set the original bouncing message to.
	 */
	enum sm_state
	bounce_message(short_msg_pending *sent_msg, const char *errstr);
	
	/*
	 * See if the handset's imsi and phone number are in the HLR
	 * database yet, since if it isn't, we can't register the imsi
	 * at its cell's host:port yet.
	 */
	bool
	ready_to_register (short_msg_p_list::iterator qmsg);

	/*
	 * Register a handset's IMSI with its cell, by sending Asterisk
	 * a SIP REGISTER message that we "relay" from the cell.  (We
	 * actually originate it, but we pretend that the cell sent it to us.
	 * Actually the cell sent us an incoming shortcode SMS from an
	 * unregistered phone, which is in the queue in REGISTER_HANDSET state.)
	 */
	enum sm_state
	register_handset (short_msg_p_list::iterator qmsg);

	/* Initial handling of an incoming SMS message in the queue */
	enum sm_state
	handle_sms_message(short_msg_p_list::iterator qmsg);

	/* When a SIP response arrives, search the queue for its matching
	   MESSAGE and handle both.  */
	void
	handle_response(short_msg_p_list::iterator qmsg);

	/* Search the message queue to find a message whose tag matches.  */
	bool
	find_queued_msg_by_tag(short_msg_p_list::iterator &mymsg,
				    const char *tag, int taghash);
	/* Same, but without a known taghash. */
	bool
	find_queued_msg_by_tag(short_msg_p_list::iterator &mymsg,
				    const char *tag);

	/*
	 * Look up the hostname and port number where we should send Short
	 * Messages for the IMSI in the To address.  
	 * 
	 * This is also where we assign a new Call-ID to the message, so that
	 * re-sends will use the same Call-ID, but re-locate's (looking up the
	 * recipient's location again) will use a new one.
	 */
	enum sm_state
	lookup_uri_hostport (short_msg_pending *qmsg);

	/* 
	 * Change the From address username to a valid phone number in format:
	 *     +countrycodephonenum
	 */
	enum sm_state
	lookup_from_address(short_msg_pending *qmsg);

	/* Change the Request-URI's address to a valid IMSI.  */
	enum sm_state
	lookup_uri_imsi (short_msg_pending *qmsg);

#if 0
	/* Insert a newly incoming message into the queue. */
	void insert_new_message(std::string str) {
		short_msg_p_list smpl(1);
		(*smpl.begin()).initialize(str);
		insert_new_message (smpl);
	}
	// Insert a new message, perhaps taking responsibility for deleting
	// the "new"-allocated memory passed in.
  	void insert_new_message(int len, char * const cstr,
	     bool use_my_memory) {
		short_msg_pending smp (len, cstr, use_my_memory);
		insert_new_message (smp);
	}
	void insert_new_message(short_msg &sm) {
		short_msg_pending smp (sm);
		insert_new_message (smp);
	}
#endif
	// For memory allocation simplicity, it's easiest to create
	// new messages as a 1-entry short_msg_p_list and then move
	// them to the real list.  Note that this moves the message's list
	// entry itself off the original list (which can then be discarded).
	void insert_new_message(short_msg_p_list &smp) {
		time_sorted_list.splice (time_sorted_list.begin(), smp);
		time_sorted_list.begin()->set_state (INITIAL_STATE);
		// time_sorted_list.begin()->timeout = 0;  // it is already
		// Low timeout will cause this msg to be at front of queue.
	}
	// This version lets the initial state be set.
	void insert_new_message(short_msg_p_list &smp, enum sm_state s) {
		time_sorted_list.splice (time_sorted_list.begin(), smp);
		time_sorted_list.begin()->set_state (s);
		// time_sorted_list.begin()->timeout = 0;  // it is already
		// Low timeout will cause this msg to be at front of queue.
	}
	// This version lets the state and timeout be set.
	void insert_new_message(short_msg_p_list &smp, enum sm_state s, 
			time_t t) {
		time_sorted_list.splice (time_sorted_list.begin(), smp);
		time_sorted_list.begin()->set_state (s, t);
	}
#if 0
	void insert_new_message(short_msg_pending &smp) {
--!	FIXME!!  This seems to COPY the smp rather than INSERT it!
		time_sorted_list.push_front (smp);
		time_sorted_list.begin()->set_state (INITIAL_STATE);
		// time_sorted_list.begin()->timeout = 0;  // it is already
		// Low timeout will cause this msg to be at front of queue.
	}
#endif

	/* Debug dump of the queue and the SMq class in general. */
	void debug_dump();

	// Set the linktag of "newmsg" to point to oldmsg.
	void set_linktag(short_msg_p_list::iterator newmsg,
			 short_msg_p_list::iterator oldmsg);

	// Get the old message that this message links to.
	bool get_link(short_msg_p_list::iterator &oldmsg,
		      short_msg_p_list::iterator qmsg);

	/*
 	 * When we reset the state and timestamp of a message,
	 * we need to reinsert it into the queue.  C++ doesn't seem
	 * to have a standard container that lets one do this cleanly.
	 * Even with lists it's a kludge -- we have to do our own search.
	 * With multisets you can't splice an element out while keeping the
	 * element... etc.
	 */
	void set_state(short_msg_p_list::iterator sm, enum sm_state newstate) {
		short_msg_p_list temp;
		/* Extract the current sm from the time_sorted_list */
		temp.splice(temp.begin(), time_sorted_list, sm);
		sm->set_state(newstate);
		// Insert it according to the new timestamp.
		// One would think that "multisets" could do this simply,
		// but they don't appear to have this capability.
		// Note: if list is empty, or all are too early, insert at end.
		for (short_msg_p_list::iterator x = time_sorted_list.begin(); 
		     true; x++) {
			if (x == time_sorted_list.end() 
			    || x->next_action_time >= sm->next_action_time) {
				time_sorted_list.splice(x, temp);
				break;
			}
		}
	};

	/*
 	 * When we reset the state and timestamp of a message,
	 * we need to reinsert it into the queue.  C++ doesn't seem
	 * to have a standard container that lets one do this cleanly.
	 * Even with lists it's a kludge -- we have to do our own search.
	 * With multisets you can't splice an element out while keeping the
	 * element... etc.
	 */
	void set_state(short_msg_p_list::iterator sm, enum sm_state newstate,
		time_t timestamp) {
		short_msg_p_list temp;
		/* Extract the current sm from the time_sorted_list */
		temp.splice(temp.begin(), time_sorted_list, sm);
		sm->set_state(newstate, timestamp);
		// Insert it according to the new timestamp.
		// One would think that "multisets" could do this simply,
		// but they don't appear to have this capability.
		// Note: if list is empty, or all are too early, insert at end.
		for (short_msg_p_list::iterator x = time_sorted_list.begin(); 
		     true; x++) {
			if (x == time_sorted_list.end() 
			    || x->next_action_time >= sm->next_action_time) {
				time_sorted_list.splice(x, temp);
				break;
			}
		}
	};

	/* Save the queue to a file; read it back from a file.
	   Reading a queue file doesn't delete things that might already
 	   be in the queue; if you want a clean queue, delete anything
	   already in the queue first.  */
	bool
	save_queue_to_file(std::string qfile);
	bool
	read_queue_from_file(std::string qfile);
};

} // namespace SMqueue

#endif

