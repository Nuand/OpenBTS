#!/bin/sh

# Install subscriber registry and assocated files.

if [ "$USER" != "root" ]; then
        echo This script must be run as super-user.
        exit 1
fi


IPATH=/usr/local/bin
cp comp128 $IPATH
cp hexmapper $IPATH
cp syslogextractor $IPATH

CGIPATH=/var/cgi-bin
cp srmanager.cgi $CGIPATH
cp subscriberserver.cgi $CGIPATH

cp comp128 /OpenBTS
cp sipauthserve /OpenBTS
cp runloop.sipauthserve.sh /OpenBTS

# Don't do this.  This will be created when the SR starts running.
#DBPATH=/var/lib/asterisk/sqlite3dir
#sqlite3 $DBPATH/sqlite3.db ".read configFiles/subscriberRegistryInit.sql"
#cp configFiles/subscriberRegistryInit.sql $DBPATH
