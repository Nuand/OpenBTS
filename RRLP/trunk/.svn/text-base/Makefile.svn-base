# This makefile is independant from the autoconf system

# default target will generate erlang modules for parsing RRLP
# and run tests on them

.PHONY: tests clean compile

compile: testall.erl RRLP.erl xrrlp.erl parseargsbody.erl util.erl
	erl -s make all -s erlang halt

all: tests

clean:
	rm -f RRLP.erl RRLP.hrl *.beam *.asn1db

# asn -> {erl,hrl}
RRLP.erl: RRLP.set.asn
	erlc -buper_bin +"{inline,optimize,'RRLP'}" RRLP.set.asn

testall.beam: testall.erl
	erlc testall.erl

tests: compile
	./testall.sh

# setup web server on mac
mweb:
	erlc rrlpserver.erl
	cp rrlpserver.cgi ~/Sites

# setup web server on itx
iweb:
	erlc rrlpserver.erl
	cp rrlpserver.cgi /var/www

tgs:
	~/bin/erltags rrlpserver.erl
	mv tags .tags1
	~/bin/asntags *.asn
	mv tags .tags2
	sort .tags1 .tags2 > tags
	rm .tags1 .tags2
	
