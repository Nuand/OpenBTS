PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS CONFIG ( KEYSTRING TEXT UNIQUE NOT NULL, VALUESTRING TEXT, STATIC INTEGER DEFAULT 0, OPTIONAL INTEGER DEFAULT 0, COMMENTS TEXT DEFAULT '');
INSERT OR IGNORE INTO "CONFIG" VALUES('SubscriberRegistry.A3A8','./comp128',0,0,'Path to the program that implements the A3/A8 algorithm.');
INSERT OR IGNORE INTO "CONFIG" VALUES('SubscriberRegistry.Manager.Title','Subscriber Registry',0,0,'Title text to be displayed on the subscriber registry manager.');
INSERT OR IGNORE INTO "CONFIG" VALUES('SubscriberRegistry.Port','5064',0,0,'Port used by the SIP Authentication Server. NOTE: In some older releases (pre-2.8.1) this is called SIP.myPort.');
INSERT OR IGNORE INTO "CONFIG" VALUES('SubscriberRegistry.UpstreamServer','',0,0,'URL of the subscriber registry HTTP interface on the upstream server.  By default, this feature is disabled.  To enable, specify a server URL eg: http://localhost/cgi/subreg.cgi.  To disable again, execute "unconfig SubscriberRegistry.UpstreamServer".');
INSERT OR IGNORE INTO "CONFIG" VALUES('SubscriberRegistry.db','/var/lib/asterisk/sqlite3dir/sqlite3.db',0,0,'The location of the sqlite3 database holding the subscriber registry.');
COMMIT;


