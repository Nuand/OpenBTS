-- Dynamic tables

CREATE TABLE `register` (
  `imsi` varchar(20) NOT NULL,
  `msisdn` varchar(24) DEFAULT NULL,
  `location` varchar(120) DEFAULT NULL,
  `updated` datetime DEFAULT NULL
)

CREATE TABLE `text_sms` (
  `imsi` varchar(20) NOT NULL,
  `msisdn` varchar(20) DEFAULT NULL,
  `dest` varchar(20) DEFAULT NULL,
  `next_try` datetime DEFAULT NULL,
  `tries` tinyint(4) DEFAULT NULL,
  `msg` varchar(160) DEFAULT NULL,
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) 
