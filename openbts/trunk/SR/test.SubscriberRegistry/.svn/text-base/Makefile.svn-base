TRUNK=../../../
COM=$(TRUNK)/CommonLibs/trunk/
SQL=$(TRUNK)/sqlite3/trunk/
LOCALLIBS=$(COM)/Logger.cpp $(COM)/Timeval.cpp $(COM)/Threads.cpp $(COM)/Sockets.cpp $(COM)/Configuration.cpp $(SQL)/sqlite3util.cpp
LIBS=$(LOCALLIBS) -losipparser2 -losip2 -lc -lpthread -lsqlite3
INCLUDES=-I$(COM) -I$(SQL)
CPPFLAGS=-g -Wall -Wno-deprecated

test: test.cpp ../SubscriberRegistry.cpp
	g++ -o test $(CPPFLAGS) $(INCLUDES) test.cpp ../SubscriberRegistry.cpp $(LIBS)
