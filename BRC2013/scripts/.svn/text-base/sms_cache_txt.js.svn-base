/**
 * sms_cache_txt.js
 * This file is based on the YATE Project http://YATE.null.ro
 *
 * SIP SMS caching proxy to text/plain SMSC implemented in Javascript
 *
 * Yet Another Telephony Engine - a fully featured software PBX and IVR
 * Copyright (C) 2012 Null Team
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.
 */

// Defaults that can be overridden by config
//HACK - fix this when Tropo is set up
is_online =true;

is_congested = false;
max_queued = 30;
sms_attempts = 60;
retry_time = sqlStr("00:02:00");
debug = false;
delivery_count = 0;
max_delivery_count = 15;

#require "bman.js"
#require "libsql.js"


// Convert a number to MSISDN (international format)
function toMSISDN(num,cc,ton)
{
	switch (ton) {
		case 0x11:
		case "international":
			return num;
		case 0x21:
		case "national":
			// ISDN national
			return cc + num;
	}
	switch (num) {
		case %+.%:
			// E.164 +CCNNNN
			return num.substr(1);
		case %00.%:
			// ITU 00CCNNNN
			return num.substr(2);
		case %011.%:
			// USA 011CCNNNN
			return num.substr(3);
		case %0z.%:
			// 0NNNN various national
			return cc + num.substr(1);
		case %z.%:
			// NNNN various national
			return cc + num;
	}
	return null;
}

// Perform one command line completion
function oneCompletion(msg,str,part)
{
	if (part != "" && str.indexOf(part) != 0)
		return;
	var ret = msg.retValue();
	if (ret != "")
		ret += "\t";
	msg.retValue(ret + str);
}

/* vi: set ts=8 sw=4 sts=4 noet: */

// Deliver SMS to registered MS
function localDelivery(id,location)
{
	Engine.debug(Engine.DebugInfo,"Attempting local delivery of #" + id + " to " + location);
	var res = rowQuery("SELECT imsi,msisdn,dest,msg FROM text_sms WHERE id=" + sqlNum(id));
	if (!res) {
		Engine.debug(Engine.DebugInfo,"Message to " + location + " missing or database locked.");
		return null;
	}
	Engine.debug(Engine.DebugInfo,"Delivery proceeding to " + location);
	sqlQuery("UPDATE text_sms SET tries=tries-1,next_try=ADDTIME(NOW()," + retry_time + ") WHERE id=" + sqlNum(id)) + " AND tries>0";

	var m = new Message("xsip.generate");
	m.method = "MESSAGE";
	m.uri = location.substr(4);
	m.user = res.msisdn;
	//m.user = "+" + res.msisdn;
	if (my_sip)
		m.domain = my_sip;
	m.xsip_type = "text/plain";
	m.xsip_body = res.msg;
	m.wait = true;
	if (m.dispatch(true)) {
		switch (m.code) {
			case 200:
			case 202:
				Engine.debug(Engine.DebugInfo,"Removing message delivered to " + location);
				sqlQuery("UPDATE text_sms SET next_try=NOW(),tries=-1 WHERE id=" + id);
			default:
				var query = "Delivery to " + location + " failed; will try again later.";
				Engine.debug(Engine.DebugInfo,query);
			return;
		}
	}
}

// Deliver SMS to upstream SMSC
function smscDelivery(id)
{
	Engine.debug(Engine.DebugInfo,"Attempting delivery of #" + id + " to Tropo");
	var res = rowQuery("SELECT imsi,msisdn,dest,msg FROM text_sms WHERE id=" + sqlNum(id));
	if (!res) {
		Engine.debug(Engine.DebugInfo,"Message to " + location + " missing or database locked.");
		return;
	}
	Engine.debug(Engine.DebugInfo,"Delivery proceeding to Tropo");
	sqlQuery("UPDATE text_sms SET tries=tries-1,next_try=ADDTIME(NOW()," + retry_time + ") WHERE id=" + sqlNum(id)) + " AND tries>0";

	var m = new Message("xsip.generate");
	m.method = "MESSAGE";
	m.uri = "sip:" + res.dest + "@" + vlr_sip;
	//m.uri = "sip:+" + res.dest + "@" + vlr_sip;
	m.user = res.msisdn;
	if (my_sip)
		m.domain = my_sip;
	m.xsip_type = "text/plain";
	// HACK : replace spaces in messages so Tropo doesn't choke
	//m.xsip_body = res.msg;
	var smsbody = res.msg;
	if (res.msisdn.length > 7) {
		if (smsbody.indexOf(" ") != -1) {
			smsbody = smsbody.split(" ");
			smsbody = smsbody.join("_");
		}
	}
	m.xsip_body = smsbody;
	m.wait = true;
	if (m.dispatch(true)) {
		switch (m.code) {
			case 200:
			case 202:
				// Success
				sqlQuery("UPDATE text_sms SET next_try=NOW(),tries=-1 WHERE id=" + id);
				return;
			case 403:
			case 404:
			case 415:
			case 488:
				// Terminal failure
				Engine.debug(Engine.DebugWarn,"Failed to submit SMS with ID=" + id + ", code=" + m.code);
				sqlQuery("UPDATE text_sms SET next_try=NOW(),tries=-3 WHERE id=" + id);
				return;
		}
	}
}

// this handel sending the SMS via SIP 
function smsSend(to,from,location,msg)
{
	Engine.debug(Engine.DebugInfo,"sendSMS trying to:" + to + "@" + location + " from:" + from);
	Engine.debug(Engine.DebugInfo,"sendSMS 2 trying to:" + to + "@" + location.substr(4) + " from:" + from);

	var m = new Message("xsip.generate");
	m.method = "MESSAGE";
	m.uri = location.substr(4);
	m.user = to;
	m.xsip_type = "text/plain";
	m.xsip_body = msg;
	m.wait = true;
	if (m.dispatch(true)) {
		switch (m.code) {
			case 200:
			case 202:
				Engine.debug(Engine.DebugInfo,"sendSMS OK to:" + to + "@" + location + " from:" + from);
				return 0;
			default:
				Engine.debug(Engine.DebugInfo,"sendSMS Fail" + m.code + " to:" + to + "@" + location + " from:" + from);
				return -1;
		}
		Engine.debug(Engine.DebugInfo,"sendSMS Timeout to:" + to + "@" + location + " from:" + from);
		return 1;
	}
}

// MO SMS handling
// Inserts an MO SMS into the database
// IMSI is the IMSI of the sender.
function moSipSms(msg,imsi)
{
	Engine.debug(Engine.DebugAll,"imsi '" + imsi );
	// IMSI here is the IMSi of the sender.

	// Don't allow sending without a return path.
	var simsi = sqlStr(imsi);
	var msisdn = valQuery("SELECT msisdn FROM register WHERE imsi=" + simsi);
	if (msisdn == "") {
		Engine.debug(Engine.DebugAll,"Message from " + imsi + " with no return address");
		msg.retValue(403); // forbidden
	}
	var dest = msg.called;
	Engine.debug(Engine.DebugAll,"MO SMS '" + imsi + "' (" + msisdn + ") -> '" + dest + "'");
	// Put the SMS into the delivery database.
	// HACK : avoid dropping single quotes into the db, should be handled with base64 in the end
	var smsbody = msg.xsip_body;
	// HACK OF A HACK : why does YATE's engine not like this, oh well, split().join() is functionally equiv
	//smsbody = smsbody.replace(/\'/g,"");
	if (smsbody.indexOf("'") != -1) {
		smsbody = smsbody.split("'");
		smsbody = smsbody.join("");
	}
	var query = "INSERT INTO text_sms(imsi,msisdn,dest,next_try,tries,msg)";
	query += " VALUES(" + simsi + "," + sqlStr(msisdn) + ","
		+ sqlStr(dest) + ",NOW()," + sqlNum(sms_attempts) + ","
		+ sqlStr(smsbody) + ")";
	query += "; SELECT LAST_INSERT_ID()";
	Engine.debug(Engine.DebugInfo,query);
	var id = valQuery(query);
	if (!id) {
		msg.retValue(500); // internal server error
	}

	msg.retValue(202); // accepted

	// try fast delivery
	//moveSms(id,true);

	return true;
}



// MT SMS are forwarded directly to OpenBTS
function mtSipSms(msg,imsi)
{
	var res = rowQuery("SELECT COALESCE(location) AS location,msisdn FROM register WHERE imsi=" + sqlStr(imsi));
	if (res) {
		if (debug)
			Engine.debug(Engine.DebugAll,"MT SMS '" + msg.caller + "' -> '" + imsi + "' (" + res.msisdn + ")");
			//Engine.debug(Engine.DebugAll,"MT SMS '" + msg.caller + "' -> '" + imsi + "' (+" + res.msisdn + ")");
		var m = new Message("xsip.generate");
		m.method = "MESSAGE";
		m.uri = res.location.substr(4);
		m.user = msg.caller;
		if (my_sip)
			m.domain = my_sip;
		m.xsip_type = msg.xsip_type;
		m.xsip_body = msg.xsip_body;
		m.wait = true;
		if (m.dispatch(true)) {
			msg.retValue(m.code);
			if (m.xsip_body) {
				msg.xsip_type = m.xsip_type;
				msg.xsip_body = m.xsip_body;
			}
		return true;
		}
		msg.retValue(503); // service unavailable
		return true;
	}
	msg.retValue(404); // not found
	return true;
}

// Handle SIP SMS
function onSipMessage(msg)
{
	Engine.debug(Engine.DebugInfo,"SMS " + msg.caller + " -> " + msg.called);

	if (msg.caller == "" || msg.called == "")
	return false;
	if (msg.xsip_type != "text/plain" || msg.xsip_body == "") {
	msg.retValue(415); // unsupported media type
	return true;
	}

		// For now, reject these until Tropo is connected.
	//if (msg.called.length >= 8) {
	//		msg.retValue(488); // not acceptable here
	//	return true;
	//}

	// HACK APP - text echo + signal information app
	if (msg.called == "511") {
		return local(msg);
	}

	if (msg.called.length != 7)
		return tropo(msg);

	// Emqueue the message.
	if (msg.caller.substr(0,4) == "IMSI")
		// MO
		return moSipSms(msg,msg.caller.substr(4));
	else if (msg.called.substr(0,4) == "IMSI")
		// MT
		return mtSipSms(msg,msg.called.substr(4));
	else
		// locally destined message from something other than a handset
		return local(msg);

	msg.retValue(488); // not acceptable here
	return true;
}

// Enqueue a message for local delivery.
function local (msg)
{
	var query;

	// HACK APP - text echo + signal information app
	if (msg.called == "511") {
		var imsi = msg.caller.substr(4);
		Engine.debug(Engine.DebugInfo,"511: Sending to text echo + signal information app from IMSI " + imsi);

		query = "SELECT msisdn FROM register WHERE imsi=" + sqlStr(imsi);
		Engine.debug(Engine.DebugInfo,"511: query = " + query);
		var msisdn = valQuery(query);
		if (!msisdn) {
			Engine.debug(Engine.DebugInfo,"511: no msisdn found");
			msg.retValue(404);
			return true;
		}
		Engine.debug(Engine.DebugInfo,"511: msisdn found: " + msisdn);

		for (var key in msg) {
			Engine.debug(Engine.DebugInfo,"511: msg." + key + " = " + msg[key]);
		}
		query = "INSERT INTO text_sms(imsi,msisdn,dest,next_try,tries,msg)";
		query += " VALUES(" + sqlStr(imsi) + "," + sqlStr("511") + ","
			+ sqlStr(msisdn) + ",ADDTIME(NOW(),'00:00:10'),5,"
			+ sqlStr(msg.xsip_body + " ::: ") + ")";
		query += "; SELECT LAST_INSERT_ID()";
		Engine.debug(Engine.DebugInfo,"511: query2 = " + query);
		var id = sqlQuery(query);
		if (!id) {
			Engine.debug(Engine.DebugInfo,"511: no id found");
			msg.retValue(500); // internal server error
			return true;
		}
		Engine.debug(Engine.DebugInfo,"511: id found");

		msg.retValue(202); // accepted

		// try immediate delivery
		//moveSms(id,true);
		//Engine.debug(Engine.DebugInfo,"511: past moveSms()");

		return true;
	}

	Engine.debug(Engine.DebugInfo,"Sending to local from IMSI " + msg.caller + " to " + sqlStr(msg.called));
	query = "SELECT imsi FROM register WHERE msisdn=" + sqlStr(msg.called);
	imsi = valQuery(query);
	if (!imsi) {
		msg.retValue(404);
		return true;
	}
	//
	//
	// Put the SMS into the delivery database.
	simsi = sqlStr(imsi);
	var smsbody = msg.xsip_body;
	if (smsbody.indexOf("'") != -1) {
		smsbody = smsbody.split("'");
		smsbody = smsbody.join("");
	}
	query = "INSERT INTO text_sms(imsi,msisdn,dest,next_try,tries,msg)";
	query += " VALUES(" + sqlStr(imsi) + "," + sqlStr(msg.caller) + ","
		+ sqlStr(msg.called) + ",NOW()," + sqlNum(sms_attempts) + ","
		+ sqlStr(smsbody) + ")";
	query += "; SELECT LAST_INSERT_ID()";
	var id = valQuery(query);
	if (!id) {
		msg.retValue(500); // internal server error
		return true;
	}

	msg.retValue(202); // accepted

	// try immediate delivery
	//moveSms(id,true);

	return true;

}



function tropo (msg)
{
	Engine.debug(Engine.DebugInfo,"Enqueing for tropo from IMSI " + msg.caller);
	var tmp = msg.caller.substr(4);
	// Set the caller ID
	if (msg.caller.match(/IMSI/)) {
		var query = "SELECT msisdn FROM register WHERE imsi=" + sqlStr(msg.caller.substr(4));
		res = rowQuery(query);
		if (res) { 
			msg.caller = res.msisdn;
			msg.callername =  res.msisdn;
		}
	}
	return moSipSms(msg,tmp);
}


// Run expiration and retries
function onInterval()
{
	if (delivery_count > max_delivery_count)
		return false;
	var m = new Message("idle.execute");
	m.module = "sms_cache";
	m.enqueue();
	nInterval.nextIdle = Date.now()/1000 + 1;
	return true;
}

// Execute idle loop actions
// This function delivers pending messages.
function onIdleAction()
{
	// count in-progress attempts
	if (delivery_count > max_delivery_count)
		return false;

	delivery_count++;
	var query = "SMS delivery loop, delivery_count=" + delivery_count;
	Engine.debug(Engine.DebugInfo,query);
	// Perform local delivery if possible
	// Get a deliverable message id.
	var query_dest = "SELECT dest,id FROM text_sms WHERE"
		+ " tries > 0 AND next_try IS NOT NULL AND NOW() > next_try"
		+ " ORDER BY next_try DESC LIMIT 1";
	var res_dest = rowQuery(query_dest);
	if (!res_dest) {
		// Nothing new pending?
		// Retry one we gave up on.
		Engine.debug(Engine.DebugInfo,"No fresh SMS ready for delivery; reviving an old one.");
		query_dest = "SELECT dest,id FROM text_sms WHERE"
			+ " tries = 0 AND next_try IS NOT NULL AND NOW() > next_try"
			+ " ORDER BY next_try DESC LIMIT 1";
		res_dest = rowQuery(query_dest);
	}
	if (!res_dest) {
		Engine.debug(Engine.DebugInfo,"No SMS ready for delivery.");
		return true;
	}
	moveSms(res_dest.id,false);

	delivery_count--;
	Engine.debug(Engine.DebugInfo,"SMS delivery loop exit, delivery_count=" + delivery_count);
	return true;
}


// Handle cache state changes
function onCacheState(msg)
{
	is_online = msg.online;
	is_congested = msg.congestion;
	return false;
}

// Perform completion of partial command lines
function onComplete(msg,line,part)
{
	switch (line) {
		case undefined:
		case "":
		case "help":
			oneCompletion(msg,"smsc",part);
			break;
		case "smsc":
			oneCompletion(msg,"list",part);
			oneCompletion(msg,"drop",part);
			oneCompletion(msg,"debug",part);
			break;
		case "smsc drop":
			var res = sqlQuery("SELECT imsi FROM text_sms GROUP BY imsi");
			if (!res)
			break;
			for (var i = 0; ; i++) {
				var user = res.getResult(i,0);
				if (user === null)
					break;
				Engine.output("Completing: " + user);
				oneCompletion(msg,user,part);
			}
			break;
		case "smsc debug":
			oneCompletion(msg,"on",part);
			oneCompletion(msg,"off",part);
			break;
	}
}

// Handle rmanager commands
function onCommand(msg)
{
	if (!msg.line) {
		onComplete(msg,msg.partline,msg.partword);
		return false;
	}
	switch (msg.line) {
		case "smsc list":
			var res = sqlQuery("SELECT msisdn,dest,imsi,tries,TIMEDIFF(next_try,NOW()) AS next FROM text_sms WHERE tries > 0");
			if (res) {
				res = res.getRow();
				var tmp = "IMSI			Originator	  Destination	 Retries Retry in\r\n";
				tmp += "--------------- --------------- --------------- ------- ---------\r\n";
				for (var i = 0; i < res.length; i++) {
					var rec = res[i];
					var nxt = rec.next;
					tmp += strFix(rec.imsi,15) + " " + strFix(rec.msisdn,15) + " "
						+ strFix(rec.dest,15) + " " + strFix(rec.tries,-7) + " " + strFix(nxt,-9) + "\r\n";
				}
				msg.retValue(tmp);
			}
			else
				msg.retValue("Database error!\r\n");
			return true;
		case /^smsc debug ./:
			switch (msg.line.substr(11)) {
			case "true":
			case "yes":
			case "on":
				debug = true;
				msg.retValue("SMS Cache Debug enabled\r\n");
				return true;
			case "false":
			case "no":
			case "off":
				msg.retValue("SMS Cache Debug disabled\r\n");
				debug = false;
				return true;
		}
	}
	return false;
}

csmsHelp = "  smsc [list|drop ID|debug on/off]\r\n";

// Provide help for rmanager command line
function onHelp(msg)
{
	if (msg.line) {
		if (msg.line == "smsc") {
			msg.retValue(csmsHelp + "Control the SMS Cache operation\r\n");
			return true;
		}
		return false;
	}
	msg.retValue(msg.retValue() + csmsHelp);
	return false;
}



function moveSms (id,fast)
{
	var query_dest = "SELECT dest FROM text_sms WHERE tries >= 0 AND id=" + sqlNum(id);
	var res_dest = rowQuery(query_dest);
	Engine.debug(Engine.DebugInfo,"Attempting delivery of #" + id + " to " + res_dest.dest);
	// 7-digit numbers are local
	if (res_dest.dest.length == 7) {
		// Get the desination IP.
		var query_loc = "SELECT location FROM register WHERE msisdn = " + sqlStr(res_dest.dest) + " LIMIT 1";
		var res_loc = rowQuery(query_loc);
		if (res_loc) {
			localDelivery(id,res_loc.location);
		} else {
			Engine.debug(Engine.Debug,"Destination " + res_dest.dest + " does not exist.");
			sqlQuery("UPDATE text_sms SET tries=0,next_try=NOW() WHERE id=" + sqlNum(id));
		}
	}
	// Everything else goes to Tropo
	else if (is_online) {
		smscDelivery(id);
	}
}


Engine.debugName("sms_cache");
Message.trackName("sms_cache");
Message.install(onCommand,"engine.command",120);
Message.install(onHelp,"engine.help",150);
Message.install(onSipMessage,"sip.message",100);
Message.install(onCacheState,"cache.status",100);
Message.install(onIdleAction,"idle.execute",110,"module","sms_cache");
Engine.setInterval(onInterval,1000);

var m = new Message("cache.query");
if (m.dispatch()) {
	is_online = m.online;
	is_congested = m.congestion;
}

/* vi: set ts=8 sw=4 sts=4 noet: */
