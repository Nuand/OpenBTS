TRUNK=../../../openbts/trunk
COM=$(TRUNK)/CommonLibs
SQL=$(TRUNK)/sqlite3
#SR=$(TRUNK)/subscriberRegistry/trunk-public-staging
LOCALLIBS=$(COM)/Logger.cpp $(COM)/Timeval.cpp $(COM)/Threads.cpp $(COM)/Sockets.cpp $(COM)/Configuration.cpp $(SQL)/sqlite3util.cpp SubscriberRegistry.cpp servershare.cpp
LIBS=$(LOCALLIBS) -losipparser2 -losip2 -lc -lpthread -lsqlite3
INCLUDES=-I$(COM) -I$(SQL)
CPPFLAGS=-g -Wall -Wno-deprecated

all: srmanager.cgi sipauthserve

srmanager.cgi: srmanager.cpp $(LOCALLIBS)
	g++ -o srmanager.cgi $(CPPFLAGS) $(INCLUDES) srmanager.cpp $(LIBS)

sipauthserve: sipauthserve.cpp $(LOCALLIBS)
	g++ -o sipauthserve $(CPPFLAGS) $(INCLUDES) sipauthserve.cpp $(LIBS)

clean:
	rm -f srmanager.cgi sipauthserve test.SubscriberRegistry/test
	rm -r -f *.dSYM
