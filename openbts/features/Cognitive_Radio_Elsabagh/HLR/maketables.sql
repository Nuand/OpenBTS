create database if not exists OpenBTS;

connect OpenBTS;

create table if not exists HRL (
	IMSI CHAR(15) NOT NULL,		-- subscriber IMSI
	MSISDN CHAR(20),		-- subscriber E.164/ISDN address
	location VARCHAR(255),		-- IP address of current Asterisk server

	-- Provisioning mask.  Each char is the state of one service.
	-- Codes:
		-- 0: no service
		-- I: inbound only
		-- L: inbound, outbound local only
		-- F: full service
	-- index 0, speech.
	-- index 1, SMS.
	provisioning VARCHAR(2) NOT NULL,
);

