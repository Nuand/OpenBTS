/*
 * SMlistener.h - Network SIP handler for Short Messages (SMS's) for OpenBTS.
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

#ifndef SM_LISTENER_H
#define SM_LISTENER_H

#include <time.h>
#include <string>
#include <iostream>
#include "poll.h"
#include <sys/socket.h>

namespace SMqueue {

class short_msg_pending;		// Forward declaration

class SMnet {
	public:
	struct pollfd *sockets;
	nfds_t allocsockets;
	nfds_t numsockets;
	int mytimeout;

	// Other things we remember about each socket
	struct extra_sockinfo {
		int addrfam;
		int socktype;
		int protofam;
		struct sockaddr_storage addr;
		socklen_t addrlen;
	} *sockinfo;

	// The source (IP-ish) address of the last packet received
	char src_addr[200];	// This is overgenerous.
	size_t recvaddrlen;	// How many bytes of src_addr is valid

	// My global hostname, based on my network address.
	char *my_network_hostname;
	// The last string of random characters returned.
	char *random_string;
	// The file descriptor we read random numbers from.
	int random_fd;

	void abfuckingort();	// where did C library abort() go?

	/* Constructor */
	SMnet() :
		sockets (NULL),
		allocsockets (0),
		numsockets (0),
		mytimeout (-1),
		sockinfo (NULL),
		recvaddrlen (0),
		my_network_hostname (0),
		random_string (0),
		random_fd (0)
	{
	}

	// We have eliminated this default copy-constructor, by making it
	// private.
	private:
	SMnet(const SMnet &) :
		sockets (NULL),
		allocsockets (0),
		numsockets (0),
		mytimeout (-1),
		sockinfo (NULL),
		recvaddrlen (0),
		my_network_hostname (0),
		random_string (0),
		random_fd (0)
	{
		abfuckingort();
	}

	/* Disable operator= so that pointers don't get shared */
	private:
	SMnet & operator= (const SMnet &rvalue);
	public:

	/* Destructor.
	   If you want to destroy this instance without closing your sockets,
	   you have to call remove_socket on them first.  */
	~SMnet() {
		unsigned i;
  		for (i = 0; i < numsockets; i++) {
			(void) close(sockets[i].fd);  // Ignore result.
		}
		delete [] sockets;
		delete [] sockinfo;
		delete [] my_network_hostname;
		delete [] random_string;
	}

	/* Add a file/network socket to the set to be polled */
	void
	add_socket(int socket, short events, int addrfam, int socktype,
		   int protofam, const struct sockaddr *addr, 
		   socklen_t addrlen) {
		if (numsockets >= allocsockets) {
			struct pollfd *sox;
			int newalloc = allocsockets + 10;

			sox = new struct pollfd[newalloc];
			memcpy(sox, sockets, numsockets * sizeof(*sockets));
			delete [] sockets;
			sockets = sox;
			allocsockets = newalloc;

			struct extra_sockinfo *si;
			si = new struct extra_sockinfo[newalloc];
			memcpy(si, sockinfo, numsockets * sizeof(*sockinfo));
			delete [] sockinfo;
			sockinfo = si;
		}
		sockets[numsockets].fd = socket;
		sockets[numsockets].events = events;
		sockets[numsockets].revents = 0;
		sockinfo[numsockets].addrfam = addrfam;
		sockinfo[numsockets].socktype = socktype;
		sockinfo[numsockets].protofam = protofam;
		if (addrlen <= sizeof(sockinfo[numsockets].addr)) {
			sockinfo[numsockets].addrlen = addrlen;
			memcpy(&sockinfo[numsockets].addr, addr, addrlen);
		} else {
			sockinfo[numsockets].addrlen = 0;
		}
		numsockets++;
	}

	/* Remove socket */
	void
	remove_socket(int socket) {
		nfds_t i, j;

		// Walk down the array, squeezing out all entries with socket
		// Note, there may be several; get rid of all.
		for (i = 0, j = 0; i < numsockets; i++) {
			if (i != j)
				sockets[j] = sockets[i];
			if (sockets[i].fd != socket) 
				j++;
		}
		numsockets = j;
		// We never shrink the allocation; it's almost peanuts.
	}

	/* Change the event mask for socket so that we'll awaken when
  	 * it's possible to write to the socket (or not).  
 	 *
	 * In normal use, we don't care about writes; writes are buffered
	 * and happen automatically.  But if a write would ever block,
	 * then our caller need to ask to be awakened when that situation stops.
	 */
	bool
	care_about_writes(int socket, bool icare) {
		nfds_t i;
		for (i = 0; i < numsockets; i++) {
			if (sockets[i].fd == socket) {
				if (icare)
					sockets[i].events |= POLLOUT;
				else
					sockets[i].events &= ~POLLOUT;
				return true;
			}
		}
		return false;
	}

	// Poll (wait for I/O or a timeout).
	// Timeout is in milliseconds; -1 for infinite.
	int
	poll_sockets(int mstimeout)
	{
		int i;

		if (mstimeout<0) mstimeout=-1;
		i = poll (sockets, numsockets, mstimeout);
		return i;
	}

	/*
	 * Initialize short-message handling. 
	 * Make one or more sockets and set up to listen on them.
	 * This function is ipv6-agnostic, and even almost UDP/TCP-agnostic.
	 */
	bool listen_on_port(std::string port);

	/* 
	 * Get the next datagram from any socket of interest.
	 * We use a system call to poll for up to mstimeout milliseconds.
	 * Result < 0:  error
	 * Result == 0: timeout, or it's OK to write to a write socket now.
	 * Result > 0: size of the datagram received.
	 */
	int get_next_dgram (char *buffer, size_t buffsize, int mstimeout);

	/*
	 * Send a datagram on a handy socket
	 * FIXME:  Make the source host/port match the one in the SIP dgram!
	 */
	bool send_dgram(char *buffer, size_t buffsize, char *toaddr,
			size_t toaddrsize);

	/*
	 * Deliver a short_msg_pending to its destination, using 
	 * the addresses in the message's request URI.
	 * Result is false if failed; true if we think we sent it.
	 * (Note that an ack should come back eventually -- we don't await it.)
	 */
	bool
	deliver_msg_datagram(short_msg_pending *);

	/*
 	 * The global name of this host (facing the network).
	 */
	char *
	myhostname();

	/* Make printable IP address */
	std::string
	string_addr (struct addrinfo *myaddr, bool withport);
	/* Make printable IP address */
	std::string
	string_addr (struct sockaddr *sa, socklen_t len, bool withport);

	/* Parse printable IP address and port */
	bool
	parse_addr (const char *str, char *sockad, socklen_t maxlen, socklen_t *len);

	/*
	 * Return a different random integer each time we are called.
	 * They are all guaranteed to be positive numbers.
	 */
	unsigned int
	new_random_number();

	/*
	 * A different random number each time it's called.  Designed
	 * to be unique for a long time, within the hostname above.
	 * Should not repeat even if the program is stopped and started.
	 */
	char *
	new_call_number();
};



}; // namespace SMqueue

#endif
