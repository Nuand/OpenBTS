/**
 * reg_cache.js
 * This file is based on the YATE Project http://YATE.null.ro
 *
 * SIP/IAX caching proxy to mobile registrar implemented in Javascript
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

#require "bman.js"
#require "libsql.js"

$ms_attempts= 20;


/* on every register request
 * 0. if the imsi belongs to att we reject the phone and tell it to go to hell
 * 1. if the imsi was never seen before we will alocate a random 5 digit number and send an sms with instructions on how change the msisdn.
 * 2. we update/set the location.
 *
 * WE DON'T CARE IF THE PHONE IS OFFLINE OR LOST IN SPACE OR IN THE PORTA POTTY.
 *
 */
function onRegister(msg)
{
	// TODO: This needs to be updated to deal with the wired phones.
	// HACK: Give the phone an IMSI for a user name.

	var query = "registration from " + msg.number;
	Engine.debug(Engine.DebugInfo,query);
	if (msg.number == "" || msg.data == "")
		return false;
	if (msg.number !== undefined) {
		if (msg.number.substr(0,4) == "IMSI")
			imsi = msg.number.substr(4);
	}

	var num = sqlStr(msg.number);
	var loc = sqlStr(msg.data);
	var imsisql = sqlStr(imsi);
	query = "SELECT imsi FROM register WHERE imsi=" + imsisql;
	Engine.debug(Engine.DebugInfo,query);
	var qres = sqlQuery(query);
	// Failed database query. Phone will try again soon.
	if (qres === null) {
			msg.retValue(503);
			query = "Failed database query. Phone will try again soon. " + qres;
			Engine.debug(Engine.DebugInfo,query);
			return true;
	}
 
	// IMSI already in use?
	var res = qres.getResult(0,0);
	if (!res) {
		// Block AT&T if not from the devkit
		// if (imsi.match(/^310(030|150|170|280|380|410|560|680|980|990)/))
		if (msg.ip_host != "10.0.5.112") {
			if (imsi.match(/^310030/))
				return false;
			if (imsi.match(/^310150/))
				return false;
			if (imsi.match(/^310170/))
				return false;
			if (imsi.match(/^310280/))
				return false;
			if (imsi.match(/^310380/))
				return false;
			if (imsi.match(/^310410/))
				return false;
			if (imsi.match(/^310560/))
				return false;
			if (imsi.match(/^310680/))
				return false;
			if (imsi.match(/^310980/))
				return false;
			if (imsi.match(/^310990/))
				return false;
		}
		else
			Engine.debug(Engine.DebugInfo,"DEVKIT --------------------------------");
		// We have a new customer!
		var num = newnumber();
		query = "INSERT INTO register (imsi,msisdn,location) VALUES (" + imsisql + "," + sqlStr(num) +"," + loc + ")";
		Engine.debug(Engine.DebugInfo,query);
		var worked = sqlQuery(query);
		if (!worked) {
			msg.retValue(503);
			return true;
		}
		message("Welcome to Legba! Your number is " + num + "; use with friends it on the playa. NO EMERGENCY CALLS!", imsi,num);
		Engine.debug(Engine.DebugInfo,query);
		msg.retValue(200);
		return true;
	}

	// Update location.
	query = "UPDATE register SET location=" + loc + " WHERE imsi=" + imsisql;
	Engine.debug(Engine.DebugInfo,query);
	worked = sqlQuery(query);
	if (!worked) {
		msg.retValue(503);
	}
	msg.retValue(200);
	return true;
}



function randomint(modulus)
{
	if (randomint.counter==undefined) {
	
		var d = new Date();
		randomint.count = d.getSeconds()*1000 + d.getMilliseconds();
	}
	randomint.count++;
	// Knuth's integer hash.
	var hash =(randomint.count * 2654435761) % 4294967296;
	return hash % modulus;
}


function goodnumber()
{
	var An = 2 + randomint(8);
	var A = An.toString();
	var Bn = randomint(10);
	var B = Bn.toString();
	var Cn = randomint(10);
	var C = Cn.toString();
	var Dn = randomint(10);
	var D = Dn.toString();
	var En = randomint(10);
	var E = En.toString();

	switch (randomint(25)) {
			// 4 digits in a row - There are 10,000 of each.
		case 0: return A+B+C+D+D+D+D;
		case 1: return A+B+C+C+C+C+D;
		case 2: return A+B+B+B+B+C+D;
		case 3: return A+A+A+A+B+C+D;
			// ABCCBA palidromes - There are about 10,000 of each.
		case 4: return A+B+C+C+B+A+D;
		case 5: return A+B+C+D+D+C+B;
			// ABCABC repeats - There are about 10,000 of each.
		case 6: return A+B+C+A+B+C+D;
		case 7: return A+B+C+D+B+C+D;
		case 8: return A+B+C+D+A+B+C;
			// AABBCC repeats - There are about 10,000 of each.
		case 9: return A+A+B+B+C+C+D;
		case 10: return A+B+B+C+C+D+D;
			// AAABBB repeats - About 1,000 of each.
		case 11: return A+A+A+B+B+B+C;
		case 12: return A+A+A+B+C+C+C;
		case 13: return A+B+B+B+C+C+C;
		// 4-digit straights - There are about 1,000 of each.
		case 14: return "2345"+B+C+D;
		case 15: return "3456"+B+C+D;
		case 16: return "4567"+B+C+D;
		case 17: return "5678"+B+C+D;
		case 18: return "6789"+B+C+D;
		case 19: return A+B+C+"1234";
		case 20: return A+B+C+"2345";
		case 21: return A+B+C+"3456";
		case 22: return A+B+C+"4567";
		case 23: return A+B+C+"5678";
		case 24: return A+B+C+"6789";
	}
}

function numberavailable(val)
{
	var query = "SELECT msisdn FROM register WHERE msisdn=" + sqlStr(val);
	var res = sqlQuery(query);
	if (res === null)
		return false;
	if (res.getResult(0,0) === null)
		return true;
	return false;
}

function newnumber()
{
	val = goodnumber();
	while (!numberavailable(val)) {
		val = goodnumber();
		//Engine.debug(Engine.DebugInfo,val);
	}
	return val;
}

function message(text,imsi,dest)
{
	
	var query = "INSERT INTO text_sms(imsi,msisdn,dest,next_try,tries,msg)";
	query += " VALUES(" + sqlStr(imsi) + ",'6611',"
		+ sqlStr(dest) + ",NOW()," + sqlNum(sms_attempts) + ","
		+ sqlStr(text) + ")";
	query += "; SELECT LAST_INSERT_ID()";
	Engine.debug(Engine.DebugInfo,"this is the message " + query);
	var id = valQuery(query);

}

Engine.debugName("bman_regist");
Message.trackName("bman_regist");
Message.install(onRegister,"user.register",80);
