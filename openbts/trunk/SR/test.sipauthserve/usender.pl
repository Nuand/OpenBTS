#!/usr/bin/perl -w

$|++;
use strict;
use IO::Socket;

my($host) = '127.0.0.1';
my($port) = '5063';

my($datagram) = "";
while (<>) {
	$datagram .= $_;
}
my $message = IO::Socket::INET->new(Proto=>"udp",
	PeerPort=>$port,PeerAddr=>$host) 
	  or die "Can't make UDP socket ($host:$port): $@";
# printf "sending $datagram";
$message->send($datagram);


# MESSAGE sip:user2@domain.com SIP/2.0
# Via: SIP/2.0/TCP user1pc.domain.com;branch=z9hG4bK776sgdkse
# Max-Forwards: 70
# From: sip:user1@domain.com;tag=49583
# To: sip:user2@domain.com
# Call-ID: asd88asd77a@1.2.3.4
# CSeq: 1 MESSAGE
# Content-Type: text/plain
# Content-Length: 18
# 
# Watson, come here.
