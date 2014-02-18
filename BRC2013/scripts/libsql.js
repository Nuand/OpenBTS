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

// Make a SQL query, return the holding message or null if the query failed
function sqlQuery(query,account)
{
    var m = new Message("database");
    if (account === undefined)
	account = dbacc;
    m.account = account;
    m.query = query;
    if (m.dispatch(!!sqlQuery.async)) {
	if (!m.error)
	    return m;
	Engine.debug(Engine.DebugWarn,"Query " + m.error + " on '" + account + "': " + query);
    }
    else
	Engine.debug(Engine.DebugWarn,"Query not handled by '" + account + "': " + query);
    return null;
}

// Make a SQL query, return 1st column in 1st row, null if query failed or returned no records
function valQuery(query)
{
    var res = sqlQuery(query);
    if (!res)
	return null;
    return res.getResult(0,0);
}

// Make a SQL query, return 1st row as Object, null if query failed or returned no records
function rowQuery(query)
{
    var res = sqlQuery(query);
    if (!res)
	return null;
    return res.getRow(0);
}

// Make a SQL query, return 1st column as Array, null if query failed or returned no records
function colQuery(query)
{
    var res = sqlQuery(query);
    if (!res)
	return null;
    return res.getColumn(0);
}

function sqlStr(str)
{
    if (str === null || str === undefined)
	return "NULL";
    return "'" + str + "'";
}

function sqlNum(num)
{
    if (num === null || num === undefined)
	return "NULL";
    return "" + num;
}

// Remove quotes around a string
function cutQuotes(str) {
    if (str === null || str === undefined)
	return null;
    if (str.charAt(0) == '"' && str.charAt(str.length - 1) == '"')
	return str.substr(1,str.length - 2);
    return str;
}

// Helper that returns a left or right aligned fixed length string
function strFix(str,len)
{
    if (str === null)
	str = "";
    if (len < 0) {
	// right aligned
	len = -len;
	if (str.length >= len)
	    return str.substr(str.length - len);
	while (str.length < len)
	    str = " " + str;
    }
    else {
	// left aligned
	if (str.length >= len)
	    return str.substr(0,len);
	while (str.length < len)
	    str += " ";
    }
    return str;
}

/* vi: set ts=8 sw=4 sts=4 noet: */
