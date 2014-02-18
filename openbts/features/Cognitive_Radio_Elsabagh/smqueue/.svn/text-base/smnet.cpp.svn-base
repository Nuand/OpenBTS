/*
 * Smnet.cpp - Network SIP listener for Short Messages (SMS's) for OpenBTS.
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

#include <time.h>
#include <osipparser2/osip_message.h>	/* from osipparser2 */
#include <iostream>
#include <fstream>
#include "poll.h"
#include <sys/types.h>			// for sys/socket.h
#include <sys/socket.h>			// recvfrom
#include <netdb.h>			// getaddrinfo
#include <errno.h>
#include <fcntl.h>
#include <cstdlib>			// l64a
#include <arpa/inet.h>			// inet_ntop

#include "smnet.h"
#include "smqueue.h"

using namespace std;

namespace SMqueue {

// Because the abort function isn't always accessible in C--?
void
SMnet::abfuckingort()
{
	*((char *)0) = -1;
}


/*
 * Send a datagram on a handy socket
 * FIXME:  Make the source host/port match the one in the SIP dgram!
 *	   Currently we just send on the first socket with right addr length!
 * Result is true if sent OK; false if not (and errno is set if false).
 */
bool
SMnet::send_dgram(char *buffer, size_t buffsize, char *toaddr,
		size_t toaddrsize)
{
	ssize_t i;
	int flags = MSG_DONTWAIT;

	if (!sockets || numsockets == 0) {
		errno = EBADF;
		return false;
	}

	nfds_t j;
	for (j = 0; j < numsockets; j++) {
		if (
		    sockinfo[j].socktype == SOCK_DGRAM &&
		    sockinfo[j].addrlen == toaddrsize) {
			i = sendto (sockets[j].fd, buffer, buffsize,
				flags, (struct sockaddr *)toaddr, toaddrsize);
			if (i < 0)
				continue;
			if (i != (ssize_t) buffsize) {
				errno = EPIPE;		// We could do better...
				return false;
			}
			return true;		// Success!
		}
	} // else try another socket

	errno = EBADF;
	return false;		// No socket worked.
}


/* Talk to the GSM engine... */
bool
SMnet::deliver_msg_datagram(SMqueue::short_msg_pending *smp)
{
	char *scheme, *host, *port;
	int s, i;

	// Make sure the text is valid before writing it for debug,
	// or delivering it to a handset.
	smp->make_text_valid();

	// FIXME, kludge it.
	// Write to first file descriptor...
	// we should chk for matching address family, etc...
	LOG(DEBUG) << endl << "--Deliver message:";
	LOG(DEBUG) << smp->text;

	// We need to have at least ONE socket open (for sending on).
	if (!sockets || numsockets == 0)
		return false;

	/* Get the addressing info (and the SIP msg itself) out of the msg */
	if (!smp->parse()) return false;
	scheme = smp->parsed->req_uri->scheme;
	host   = smp->parsed->req_uri->host;
	port   = smp->parsed->req_uri->port;
	
	struct addrinfo myhints;
	struct addrinfo *myaddrs, *ap;

	memset(&myhints, 0, sizeof(myhints));
	myhints.ai_family = AF_UNSPEC;		// Any address family eg v4/6
	myhints.ai_socktype = SOCK_DGRAM;	// Datagrams for now FIXME
#ifdef AI_IDN
	myhints.ai_flags = AI_IDN;		// Int'l dom names OK.
#endif
	if (!scheme)
		scheme = (char * const)"sip";
	if (!port)
		port = scheme;	// More specific port is better

	s = getaddrinfo(host, port, &myhints, &myaddrs);
	if (s != 0) {
		LOG(ERROR) << "deliver_msg_datagram() can't lookup addr/port: " 
		     << host << ":" << port << ", error " << s;
		return false;
	}
	
	// Now deliver to some working address we got back.
	// For each address, find an open socket that has the right
	// kind of protocol and return address...
	// FIXME, do we need to match the bound address with the packet's
	// "return address"?  Maybe...
	for (ap = myaddrs; ap != NULL; ap = ap->ai_next) {
		int flags = MSG_DONTWAIT;
		nfds_t j;
		i = 0;
		for (j = 0; j < numsockets; j++) {
			if (sockinfo[j].addrfam == ap->ai_family &&
			    sockinfo[j].socktype == ap->ai_socktype &&
			    sockinfo[j].protofam == ap->ai_protocol &&
			    sockinfo[j].addrlen == ap->ai_addrlen) {
				i = sendto (sockets[j].fd, 
					smp->text, smp->text_length,
					flags, ap->ai_addr, ap->ai_addrlen);
				if (i < 0) {
					break;	// Try another address
				}
			}
		}
		if (i < 0) continue;		// Errored, try another address
		if (i == 0) continue;		// No match, try another address.
		
		break;				// sent -- stop trying!
	}
	freeaddrinfo(myaddrs);		// Don't leak memory.
	if (ap == NULL) {		// If we walked off bottom of list,
		LOG(ERROR) << "Couldn't send datagram to " << host << ":" 
		     << port << " on any socket";
		return false;		// we failed to send
	}
	return true;			// Hey, we sent the message onward!
}


/* 
 * Network listener for SIP Short Messages.
 * Timeout in milliseconds (negative means infinity).
 *
 * Result is <0 if error; 
 * Result is >0 if buffer contains a received packet.
 * (Packet's source address is saved away for future access by
 * our caller.)
 * Result == 0 if caller was interested in writing, there's no received
 * packet yet, and it's OK to write now.  OR if we timed out.
 */
int
SMnet::get_next_dgram (char *buffer, size_t bufferlen, int mstimeout)
{
	int i, fd, flags;
	nfds_t j;
	short revents;
	socklen_t addrlen; 
	size_t recvlength;

	i = poll_sockets(mstimeout);
	if (i < 0) 	// error
		return i;
	if (i == 0)	// timeout
		return 0;

	for (j = 0; j < numsockets; j++) {	// Walk the sockets.
		fd = sockets[j].fd;
		revents = sockets[j].revents;

#ifdef POLLRDHUP
		if (revents & (POLLIN|POLLPRI|POLLRDHUP))
#else
		if (revents & (POLLIN|POLLPRI))
#endif
		{					// input OK
			// FIXME, TCP and files aren't supported yet
			addrlen = sizeof(src_addr);
			flags = MSG_DONTWAIT|MSG_TRUNC;
			recvlength = recvfrom(fd, buffer, bufferlen, flags,
					(sockaddr *)&src_addr, &addrlen);
			if (recvlength < 0) {
				// Error on receive.
				LOG(ERROR) << "Error " << strerror(errno)
				     << "on recvfrom";
				// We shouldn't loop -- or the error might make
				// an endless loop.  Return.
				return -1;
			}
			if (addrlen > sizeof(src_addr)) {
				// Received address truncated.  BUG in program!
				LOG(ERROR) << "recvfrom received address truncated!";
				// FIXME, print src_addr too
				abfuckingort();
			}
			recvaddrlen = addrlen;		// Save for later
			if (recvlength > bufferlen) {
				// Received packet itself truncated.
				LOG(ERROR) << "recvfrom data packet truncated, "
				        "buffer has " 
				     << bufferlen << " bytes, packet of " 
				     << recvlength << "!";
				// FIXME, print src_addr too
				return -1;
			}
			
			//
			// OK, we got a full packet from a particular address.
			// Pass it upstairs for further processing.
			//
			return recvlength;
		}

		if (revents & (POLLOUT)) {	// output OK
			return 0;		// Tell caller to retry write
		}

		if (revents & (POLLERR|POLLHUP|POLLNVAL)) {	// errors
			LOG(ERROR) << "Poll error " << strerror(errno)
			     << "huh!";
			return -1;
		}
		// If no bits set on this FD, loop to the next one.
	}

	LOG(ERROR) << "Poll() returned " << i << " without any pending I/O";
	return -1;
}


/*
 * Initialize short-message handling. 
 * Make one or more sockets and set up to listen on them.
 * This function is xxx ought-to-be ipv6-agnostic, and is even
 * almost UDP/TCP-agnostic.
 */
bool
SMnet::listen_on_port(std::string port)
{
	int s;
	int gotasocket = 0;
	struct addrinfo myhints;
	struct addrinfo *myaddrs, *ap;

	memset(&myhints, 0, sizeof(myhints));
	myhints.ai_family = AF_UNSPEC;		// Any address family eg v4/6
	myhints.ai_socktype = SOCK_DGRAM;	// Datagrams for now FIXME
	myhints.ai_flags = AI_PASSIVE;		// From anybody

	s = getaddrinfo(NULL, (port.c_str()), &myhints, &myaddrs);
	if (s != 0) {
		LOG(ERROR) << "listen_on_port(" << port 
		     << ") can't get addr/port to listen on";
		return -1;
	}
	
	for (ap = myaddrs; ap != NULL; ap = ap->ai_next) {
	
		int fd, i;

		fd = socket(ap->ai_family, ap->ai_socktype, ap->ai_protocol);
		if (fd < 0)
			continue;		// Try another
		// Set our port number & address.
		i = bind(fd, ap->ai_addr, ap->ai_addrlen);
		if (i < 0) {
			LOG(ERROR) << "listen_on_port(" << port
			     << ") can't bind to addr '"
			     << string_addr (ap, true) << "': " 
			     << strerror(errno);
			close(fd);		// Don't leave it dangling
			continue;		// Try another
		}
		
		(void) fcntl(fd, F_SETFL, O_NONBLOCK);	// Non-blocking I/O
#ifdef O_CLOEXEC
		(void) fcntl(fd, F_SETFD, O_CLOEXEC);	// Close on exec child
#endif

		// Now set up our class to poll on, and use, this socket.
		add_socket (fd, POLLIN|POLLPRI, ap->ai_family,
			ap->ai_socktype, ap->ai_protocol, ap->ai_addr,
			ap->ai_addrlen);
		// Be slightly verbose here.
		LOG(INFO) << "Listening at address '"
		     << string_addr (ap, true) << "'.";
		gotasocket++;
		// And keep looping to make several sockets if we can!
	}
	freeaddrinfo(myaddrs);		// Don't leak memory.

	if (!gotasocket)
		return false;
	return true;
}

/*
 * My hostname as known to the global network.
 *
 * FIXME, there is no kernel interface for finding this out.  Our return
 * address depends what interface our packets go out.  NAT and such make
 * it even more complicated.  This would need to be a config-file setting.
 */
char *
SMnet::myhostname()
{
	size_t plen = INET6_ADDRSTRLEN;
	struct sockaddr_storage sockadd;
	struct sockaddr *sp;
	char *p = 0;
	int i;
	socklen_t socklen;
	int flags;
	
	// Cache it once, return it forever.
	if (my_network_hostname)
		return my_network_hostname;

	// Get "my address" on some socket.
	sp = (struct sockaddr *)&sockadd;
	socklen = sizeof (sockadd);
	i = getsockname (sockets[0].fd, sp, (socklen_t *)&socklen);

	p = new char[plen];
	p[0] = 'x'; p[1] = '\0';	// In case of failure
	flags = NI_DGRAM|NI_NUMERICHOST|NI_NUMERICSERV;
	i = getnameinfo(sp, socklen, p, plen, (char *)0, 0, flags);
	if (i != 0) {
		LOG(ERROR) << "myhostname() can't find our name: error "
		     << gai_strerror(i);
	}

	my_network_hostname = p;
	return my_network_hostname;
}

/* Make printable IP address */
std::string
SMnet::string_addr (struct addrinfo *myaddr, bool withport)
{
	return string_addr (myaddr->ai_addr, myaddr->ai_addrlen, withport);
}

/* Make printable IP address */
std::string
SMnet::string_addr (struct sockaddr *sa, socklen_t len, bool withport)
{
	size_t plen = NI_MAXHOST;
	size_t portlen = NI_MAXSERV;
	char *p = 0, *port = 0;
	int i;
	string ret;
	int flags;

	p = new char[plen];
	p[0] = '0'; p[1] = '\0';	// In case of failure
	if (withport) {
		port = new char[portlen];
		port[0]='0'; port[1] = '\0';
	}
	if (len != 0) {
		flags = NI_DGRAM|NI_NUMERICHOST|NI_NUMERICSERV;
		i = getnameinfo(sa, len, p, plen, port, portlen, flags);
		if (i != 0) {
			LOG(ERROR) << "string_addr() can't print IP address: error "
			     << gai_strerror(i);
			if (0 == inet_ntop(sa->sa_family, sa->sa_data, p, plen)) {
				LOG(ERROR) << "string_addr() can't print IP address: inet_ntop error "
				     << strerror(errno);
			}
			//FIXME snprintf(port, portlen, "%d", sa->port);
		}
	}

	if (withport) 
		ret = string(p) + ":" + string(port);
	else
		ret = string(p);

	delete [] p;
	delete [] port;
	return ret;
}

/* Parse printable IP address and port */
bool
SMnet::parse_addr (const char *str, char *sockad, socklen_t maxlen, socklen_t *len)
{
	char *p = strchr(str, ':');
	char *host, *port;

	if (p == NULL)
		return false;

	// Empty address
	if (!strcmp("0:0", str)) {
		*len = 0;
		return true;
	}

	host = new char[1 + p - str];
	strncpy(host, str, p-str);
	host[p-str] = '\0';
	port = p+1;

	struct addrinfo myhints;
	struct addrinfo *myaddrs, *ap;
	int s;

	memset(&myhints, 0, sizeof(myhints));
	myhints.ai_family = AF_UNSPEC;		// Any address family eg v4/6
	myhints.ai_socktype = SOCK_DGRAM;	// Datagrams for now FIXME

	s = getaddrinfo(host, port, &myhints, &myaddrs);
	if (s != 0) {
		LOG(ERROR) << "parse_addr() can't lookup addr/port: " 
		     << host << ":" << port << ", error " << s;
		return false;
	}
	
	int seenone = false;
	// Now pick some working address we got back.
	for (ap = myaddrs; ap != NULL; ap = ap->ai_next) {
		if (ap->ai_addrlen > maxlen)
			continue;
		if (ap->ai_socktype != SOCK_DGRAM)
			continue;

		if (seenone) return false;
		seenone = true;

		// We found it!
		memcpy(sockad, ap->ai_addr, ap->ai_addrlen);
		*len = ap->ai_addrlen;
	}
	freeaddrinfo(myaddrs);		// Don't leak memory.
	return seenone;
}
		

/*
 * Return a different random integer each time we are called.
 * They are all guaranteed to be positive numbers.
 */
unsigned int
SMnet::new_random_number()
{
#define	RAND_DEVICE	"/dev/urandom"
	union {
		char randbuf[4];
		int randnum;
	} randy;
	static int fallback;
	int i;

	if (random_fd <= 0) {
		random_fd = open (RAND_DEVICE, O_RDONLY);
		if (random_fd < 0) {
			LOG(ERROR) << "Can't open " << RAND_DEVICE;
		}
	}

	do {
		if (random_fd > 0) {
			i = read(random_fd, &randy.randbuf, sizeof(randy.randbuf));
			if (i != sizeof(randy.randbuf)) {
				LOG(ERROR) << "Can't read from " << RAND_DEVICE;
				randy.randnum = ++fallback;
			}
		} else {
			randy.randnum = ++fallback;
		}
		
		if (randy.randnum < 0)
			randy.randnum = -randy.randnum;

	// Avoid passing either 0 or -infinity to a64l
	// (MacOS is particularly finicky about that, but it really is undef.)
	} while (randy.randnum <= 0);
	
	return (unsigned int)randy.randnum;
}

/*
 * Return a different random string (a "call number" for a Call-ID in
 * SIP, RFC 3261) each time we are called.  That string is good until the
 * next call, but may need to be copied if needed beyond that.
 */
char *
SMnet::new_call_number()
{
	char *p;
	long randnum;

	randnum = new_random_number();
	
	p = l64a (randnum);
	if (!random_string) {
		random_string = new char[6+1];
	}
	strncpy(random_string, p, 6);
	random_string[6] = '\0';
	return random_string;
}


} // namespace SMlistener
