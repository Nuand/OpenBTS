COM=CommonLibs
SQL=sqlite3
SR=.
LOCALLIBS=$(COM)/Logger.cpp $(COM)/Timeval.cpp $(COM)/Threads.cpp $(COM)/Sockets.cpp $(COM)/Configuration.cpp $(COM)/sqlite3util.cpp $(SR)/SubscriberRegistry.cpp $(COM)/Utils.cpp servershare.cpp
LIBS= -L$(SQL) $(LOCALLIBS) -losipparser2 -losip2 -lc -lpthread -lsqlite3
INCLUDES=-I$(COM) -I$(SQL) -I$(SR)
CPPFLAGS=-g -Wall -Wno-deprecated

DESTDIR := 

all: comp128 srmanager.cgi subscriberserver.cgi sipauthserve

comp128: comp128.c
	g++ -o comp128 comp128.c

subscriberserver.cgi: subscriberserver.cpp $(LOCALLIBS)
	g++ -o subscriberserver.cgi $(CPPFLAGS) $(INCLUDES) subscriberserver.cpp $(LIBS)

srmanager.cgi: srmanager.cpp $(LOCALLIBS)
	g++ -o srmanager.cgi $(CPPFLAGS) $(INCLUDES) srmanager.cpp $(LIBS)

sipauthserve: sipauthserve.cpp $(LOCALLIBS)
	g++ -o sipauthserve $(CPPFLAGS) $(INCLUDES) sipauthserve.cpp $(LIBS)

clean:
	rm -f comp128 subscriberserver.cgi srmanager.cgi sipauthserve test.SubscriberRegistry/test
	rm -r -f *.dSYM

# this needs "local7.debug<at least one tab>/var/log/openbts.log" in /etc/syslog.conf
test: all
	cd test.SubscriberRegistry; ./runtest
	cd test.sipauthserve; ./runtest
	cd test.srmanager; ./runtest
	cd test.subscriberserver; ./runtest

just3: all
	cd test.SubscriberRegistry; ./runtest
	cd test.srmanager; ./runtest
	cd test.subscriberserver; ./runtest

IPATH=$(DESTDIR)/usr/local/bin/
CGIPATH=$(DESTDIR)/var/cgi-bin/
OPATH=$(DESTDIR)/OpenBTS/
CONFIGPATH=$(DESTDIR)/etc/OpenBTS/

# specifically for boa web server
install: all
	mkdir -p $(IPATH)
	install comp128 $(IPATH)
	install hexmapper $(IPATH)
	install syslogextractor $(IPATH)
	mkdir -p $(CGIPATH)
	install -m +s -o root srmanager.cgi $(CGIPATH)
	install -m +s -o root subscriberserver.cgi $(CGIPATH)
	mkdir -p $(OPATH)
	install comp128 $(OPATH)
	install sipauthserve $(OPATH)
	install runloop.sipauthserve.sh $(OPATH)
	mkdir -p $(CONFIGPATH)
	install subscriberRegistry.example.sql $(CONFIGPATH)

