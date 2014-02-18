% Copyright 2011 Range Networks, Inc.
% Copyright 2011 Free Software Foundation, Inc.
%
% This software is distributed under the terms of the GNU Affero Public License.
% See the COPYING file in the main directory for details.
%
% This use of this software may be subject to additional restrictions.
% See the LEGAL file in the main directory for details.
%
%	This program is free software: you can redistribute it and/or modify
%	it under the terms of the GNU Affero General Public License as published by
%	the Free Software Foundation, either version 3 of the License, or
%	(at your option) any later version.
%	
%	This program is distributed in the hope that it will be useful,
%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%	GNU Affero General Public License for more details.
%	
%	You should have received a copy of the GNU Affero General Public License
%	along with this program.  If not, see <http://www.gnu.org/licenses/>.


-module(rrlpserver).
-export([run/0]).
-include("RRLP.hrl").
-include_lib("kernel/include/file.hrl").


% Here are my suggestions for reading and reference for an experienced
% programmer who is new to erlang, who wants to focus on the features
% used in rrlpserver.erl.  If you read the Readings and use the
% References, you should be off to a good start, and may even have
% all you need to mess about with the rrlp server.
% 
% Readings:
% 
% http://www.erlang.org/course/sequential_programming.html
% http://www.erlang.org/doc/programming_examples/list_comprehensions.html
% 
% Reference:
% 
% http://www.erlang.org/doc/reference_manual/expressions.html
% http://www.erlang.org/doc/man/string.html
% http://www.erlang.org/doc/man/lists.html
% http://www.erlang.org/doc/man/calendar.html
% http://www.erlang.org/doc/man/io.html


% This is an HTTP server that receives GET queries for encoding and decoding APDUs
% for RRLP, along with queries for self-testing.  The queries:
%
% input
% 	query=loc
% output
% 	apdu=<hex encoding of apdu requesting gps location, including reference location,
% 		minimum accuracy, and maximum response time>
% 
% input
% 	query=assist
% output
% 	apdu=<hex encoding of apdu containing assist information to aid the gps receiver.
% 		there are several apdu=<...> lines, due to the mobile's inability to handle segmented lines.>
% 
% input
% 	query=apdu
% 	apdu=<hex encoding of apdu to decode>
% output for ack
% 	ack=
% output for position response
% 	latitude=<latitude in degrees>
% 	longitude=<longitude in degrees>
% 	altitude=<altitude in meters>
% 	positionError=<position uncertainty in meters>
% output for protocol error
% 	error=protocolError <protocol error>
% 
% input
% 	query=decode
% 	apdu=<hex-encoded apdu>
% output
% 	bits=<bits of apdu>
% 	decode=<decoded apdu>
% 
% input
% 	query=raw
% 	apdu=<hex-encoded apdu>
% output
% 	bits=<bits of apdu>
% 	raw decode=<decoded apdu, not assuming PDU as in "decode", above>
% 
% input
% 	query=testpos
% output
% 	lists satellites and their positions
% 
% input
% 	query=testlled
% output
% 	tests encoding and decoding of latitude and longitude
% 
% optional outputs of all commands:
% error=<error string.  implies query failed>
% note=<informative note>
% info=<informative note>
% and there are other outputs that get ignored
% 
% Configuration parameters are also included in the QUERY_STRING:
% GSM.RRLP.ACCURACY
%	minimum accuracy for location request, in meters
%	K in 10(1.1**K-1). See 3GPP 03.32, sect 6.2
% GSM.RRLP.ALMANAC.ASSIST.PRESENT
% 	0=>Don't include almanac in assist apdu
% 	1=>DO include it
% GSM.RRLP.ALMANAC.REFRESH.TIME
% 	Number of hours after which to refresh almanac file
% GSM.RRLP.ALMANAC.URL
% 	URL from which to get almanac file
% GSM.RRLP.EPHEMERIS.ASSIST.COUNT
% 	Number of satellites to include in ephemeris assist data (navigation model)
% GSM.RRLP.EPHEMERIS.REFRESH.TIME
% 	Number of hours after which to refresh ephemeris file
% GSM.RRLP.EPHEMERIS.URL
% 	URL from which to get ephemeris file
% GSM.RRLP.RESPONSETIME
%	maximum response time in seconds
%	N in 2**N. See 3GPP 04.31 sect A.2.2.1
%	Note that OpenBTS timeout is 130 sec = max response time + 2
% GSM.RRLP.SEED.ALTITUDE
% 	seed altitude in meters
% GSM.RRLP.SEED.LATITUDE
% 	seed latitude in degrees
% 	0=equator, 90=north pole, -90=south pole
% GSM.RRLP.SEED.LONGITUDE
% 	seed longitude in degrees
% 	0=Greenwich, +=east, -=west
	
% Execution starts in "run" because it's exported (above) and the erl
% command in rrlpserver.cgi says to start in "run".
% HTTP puts the query in environment variable QUERY_STRING.
% Queries are in the form of "key=value", separated by ampersands.
% All the queries are numbers or simple strings, except for the two URLs,
% which would mess up decoding, so the two URLs are hex-encoded.
run() ->
	io:format("Content-type: text/html\n\n"),
	case os:getenv("QUERY_STRING") of
		false -> 
			io:format("error=no QUERY_STRING\n");
		"" ->
			io:format("error=empty QUERY_STRING\n");
		QUERY_STRING -> 
			io:format("QUERY_STRING=~p\n", [QUERY_STRING]),
			% decode QUERY_STRING into list of [key,value] lists
			LofL = [string:tokens(Y,"=") || Y <- string:tokens(QUERY_STRING, "&")],
			% convert it to list of {key,value} tuples because that's what dict:from_list wants
			LofT = [list_to_tuple(X) || X <- LofL],
			% generate a dictionary
			Dict = dict:from_list(LofT),
			% put the dictionary into the "process dictionary", a global dictionary
			queryStringToProcessDictionary(LofT),
			% take a look at the query and do something with it
			case dict:find("query", Dict) of
				{ok,"testpos"} -> testpos();
				{ok,"testlled"} -> testlled();
				{ok,"raw"} -> raw(Dict);
				{ok,"decode"} -> decode(Dict);
				{ok,"assist"} -> pduOut(assist());
				{ok,"loc"} -> locate();
				{ok,"apdu"} ->
					case dict:find("apdu", Dict) of
						{ok,APDU} ->
							respond(APDU);
						error ->
							io:format("error=no apdu\n")
					end;
				{ok,_} ->
					io:format("error=unknown query\n");
				error ->
					io:format("error=no query\n")
			end
	end.

% Put the QUERY_STRING {key,value} dictionary into the (global) process dictionary.
% It's generally frowned upon to use global data in erlang, but the configuration data
% is global in OpenBTS, so I'm making it global here.
queryStringToProcessDictionary([]) -> nop;
% queryStringToProcessDictionary([[Key,Val] | Rest]) ->
queryStringToProcessDictionary([{Key,Val0}|Rest]) ->
	% check if the key has .URL in it
	case (string:str(Key,".URL")) of
		0 ->
			% it doesn't, so not conversion needed
			Val = Val0;
		_ ->
			% it does, so it's on of the URLs, needing hex decoding
			Val = hexToString(Val0)
	end,
	% this puts the key and value into the process dictionary
	erlang:put(Key, Val),
	queryStringToProcessDictionary(Rest).

hexToString([]) -> [];
hexToString([A,B|T]) ->
	% take the next two hex digits and turn them into an ascii character, and recurse
	[unhex(A)*16+unhex(B) | hexToString(T)].

% turn hex digits into their binary values
unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(C) when C >= $A, C =< $F -> C - $A + 10.

% lookup Key in the process dictionary and return Value
getStr(Key) ->
	case erlang:get(Key) of
		undefined ->
			io:format("missing configuration value for ~p\n", [Key]),
			undefined;
		Value ->
			Value
	end.

% lookey Key in the process dictionary and return numeric Value
getNum(Key) ->
	Value = getStr(Key),
	to_num(Value).

% test - apdu-encode and apdu-decode two sets of latitude and longitude
testlled() ->
	testlled(30.1234, -120.5678),
	testlled(-30.1234, 120.5678).

% apdu-encode latitude and longitude and decode, printing before and after to see if you get back the original
testlled(Lat, Long) ->
	A = encodeLatitude(Lat),
	B = encodeLongitude(Long),
	C = decodeLatitude(A),
	D = decodeLongitude(B),
	io:format("~p\n", [Lat]),
	io:format("~p\n", [C]),
	io:format("~p\n", [Long]),
	io:format("~p\n", [D]).

% display raw decode of apdu
raw(Dict) ->
	% get the apdu
	{ok,APDU} = dict:find("apdu", Dict),
	% turn it into binary
	Bin = util:hex_to_bin(APDU),
	io:format("bits=~p\n", [Bin]),
	% decode binary.  This calls a routine built by the ASN1 compiler.
	X = 'RRLP':decode('PDU', Bin),
	io:format("raw decode = ~p\n", [X]).

% Like raw, but assumes some structure to the apdu
decode(Dict) ->
	{ok,APDU} = dict:find("apdu", Dict),
	Bin = util:hex_to_bin(APDU),
	io:format("bits=~p\n", [Bin]),
	case 'RRLP':decode('PDU', Bin) of
        {ok, {'PDU', _, Component}} ->
			io:format("decode=~p\n", [Component]);
		Else ->
			io:format("error=~p\n", [Else])
	end.

% This takes a list of apdus and prints them out one per line.
% We have to break up the assist apdu into several apdus because
% the mobile devices don't handle segmentation described in the spec.
pduOut([]) -> nop;
pduOut([Pdu|Rest]) ->
	apdu(Pdu),
	pduOut(Rest).

% generate and print apdu for location request
locate() ->
	% read ephemeris because the location request includes reference time, which needs info in the ephemeris
	Ephemeris = readEphemeris(),
	% {_,Asts} = Ephemeris,
	% io:format("~p\n", [[1+dictFetch("satelliteID", Ast) || Ast <- Asts]]),
	% generate the request apdu
	% desired accuracy and response time are configuration parameters
	Req = locationRequest(getNum("GSM.RRLP.ACCURACY"), getNum("GSM.RRLP.RESPONSETIME"), Ephemeris),
	% print the apdu
	apdu(Req).

% print an apdu
apdu(APDU) ->
	% encode into binary, using routine generated by ASN1 compiler
	case xrrlp:encodePDU(APDU) of
		{ok, Binary} ->
			% hex encode
			Hex = util:bin_to_uhex_string(Binary),
			io:format("note=apdu lth = ~p bytes\n", [length(Hex)/2]),
			% print it
			io:format("apdu=~s\n", [Hex]);
		Else ->
			io:format("error=encode returns ~p\n", [Else])
	end.

% APDU is a hex-encoded response from the mobile
respond(APDU) ->
	% io:format("APDU = ~p<br>\n", [APDU]),
	% convert it to binary
	Bin = util:hex_to_bin(APDU),
    case 'RRLP':decode('PDU', Bin) of
        {ok, {'PDU', _, Component}} -> % ignoring reference number
			case Component of
				{assistanceDataAck,_} ->
					% an acknowledge to assistance data we sent
					io:format("ack=\n");
				{msrPositionRsp, MsrPositionRsp} ->
					% a position response from a position request
					positionResponse(MsrPositionRsp);
				{protocolError, ProtocolError} ->
					% a protocol error
					ErrorCause = ProtocolError#'ProtocolError'.errorCause,
					io:format("error=protocolError ~w\n", [ErrorCause]);
				_ ->
					io:format("error=unexpected apdu component: ~w\n", [Component])
			end;
		Else ->
			io:format("error=~w\n", [Else])
    end.

% generate the apdu for a location request
locationRequest(Accuracy, ResponseTime, Ephemeris) ->
	#'PDU'{referenceNumber=1,
		component={msrPositionReq, #'MsrPosition-Req'{
			% you can include all the assistance data you want within the loc req
			'gps-AssistData'=#'GPS-AssistData'{
				% we're just going with reference time
				% the rest of the assistance data goes separately
				controlHeader=#'ControlHeader'{
					referenceTime=referenceTime(Ephemeris)
				}
			},
			% here's the loc req, telling about method, accuracy, response time
			positionInstruct=#'PositionInstruct'{
				methodType={msBased, Accuracy}, % did the spec change?
				positionMethod=gps,
				measureResponseTime=ResponseTime,
				useMultipleSets=oneSet
			}}}}.


% decode the position response to our position request
positionResponse(MsrPositionRsp) ->
	io:format("info=~w\n", [MsrPositionRsp]),
	% extract the location info from the response
	Loc = MsrPositionRsp#'MsrPosition-Rsp'.locationInfo,
	% extract the position estimate from the location info
	Pos = Loc#'LocationInfo'.posEstimate,
	% lat and long are always in same place regardless of position estimate type
	Lat = decodeLatitude(string:sub_string(Pos, 2, 4)),
	Long = decodeLongitude(string:sub_string(Pos, 5, 7)),
	io:format("latitude=~w\n", [Lat]),
	io:format("longitude=~w\n", [Long]),
	% where the altitude and position error are depends on position estimate type
	% see 23.032, sect 7
	case lists:nth(1,Pos) bsr 4 of
		1 ->
			Altitude = 0,
			K = lists:nth(8, Pos),
			C = 10,
			X = 0.1,
			Error = C*(math:pow(1+X,K)-1);
		3 ->
			Altitude = 0,
			Error = lists:nth(8, Pos);
		8 ->
			Altitude = decodeAltitude(Pos),
			Error = 0;
		9 ->
			Altitude = decodeAltitude(Pos),
			Error = lists:nth(10, Pos);
		_ ->
			Altitude = 0,
			Error = 0
	end,
	io:format("altitude=~w\n", [Altitude]),
	io:format("positionError=~w\n", [Error]).

% decode altitude
% see 23.032, sect 7.3.5
decodeAltitude(Pos) ->
	Mag = ((lists:nth(8,Pos) bsl 8) bor lists:nth(9,Pos)) band 16#7fff,
	case lists:nth(8,Pos) band 16#80 == 0 of
		true -> Mag;
		false -> -Mag
	end.

% encode latitude
% see 23.032, sect 7.3.1
encodeLatitude(Lat) ->
	X = oneElseNegOne(Lat > 0) * Lat / (180 * math:pow(2, -24)),
	encodeOctets(X, 3, Lat < 0).

% decode latitude
% see 23.032, sect 7.3.1
decodeLatitude([A,B,C]) ->
	X = ((A rem 128) bsl 16) bor (B bsl 8) bor C,
	oneElseNegOne(A < 128) * X * 180 * math:pow(2, -24).

% decode longitude
% see 23.032, sect 7.3.1
decodeLongitude([A,B,C]) ->
	X = (A bsl 16) bor (B bsl 8) bor C,
	Y = X * 180 * math:pow(2, -23),
	Y - 360 * oneElseZero(Y > 180).

% encode longitude
% see 23.032, sect 7.3.1
encodeLongitude(Long) ->
	X = Long + 360 * oneElseZero(Long < 0),
	Y = X / (180 * math:pow(2, -23)),
	encodeOctets(Y, 3, false).

% encode a value into a list of octets
% if sign is true, set the sign bit in the first octet
encodeOctets(Value, Octets, Sign) ->
	X = lists:reverse(lists:seq(0,Octets-1)),
	Y = [(round(Value) bsr (8*B)) band 16#ff || B <- X],
	[hd(Y) + 128 * oneElseZero(Sign) | tl(Y)].

% a handy function to avoid case statements
oneElseZero(true) -> 1;
oneElseZero(false) -> 0.

% another handy function to avoid case statements
oneElseNegOne(true) -> 1;
oneElseNegOne(false) -> -1.

% returns a list of assistance pdu's, to be sent separately and safely to the mobile
% 
assist() ->
	% we need the ephemeris for the global data (utc and ionospheric)
	Ephemeris=readEphemeris(),
	% the small ephemeris stuff is the ref location and utc and ionospheric data
	SmallEphemerisStuff = [smallEphemerisStuff(Ephemeris)],
	% data from the almanac
	AlmanacStuff = genAlmanacStuff(),
	% navigation model - data from the ephemeris
	EphemerisStuff = genEphemerisStuff(Ephemeris),
	% put them all together into a list of pdu's
	AlmanacStuff ++ EphemerisStuff ++ SmallEphemerisStuff.

% generate ephemeris apdus
genEphemerisStuff({Global,Sats}) ->
	% get the ephemeris count (number of satellites we want) from config params
	EphemerisCount = getNum("GSM.RRLP.EPHEMERIS.ASSIST.COUNT"),
	case EphemerisCount of
		0 ->
			% if it's 0, then return empty list
			[];
		_ ->
			% otherwise take that many satellites, and break them into safe-size apdus
			genBestEphemeris({Global, lists:sublist(Sats,1,EphemerisCount)})
	end.

% recursive function that breaks list of satellites into groups of 3 (while actually generating the apdu)
genBestEphemeris({_,[]}) -> [];
genBestEphemeris({Global,Sats}) ->
	[navigationModelSubset({Global,Sats}, 1, 3) | genBestEphemeris({Global, safeNthTail(3, Sats)})].

% generate almanac apdus
genAlmanacStuff() ->
	% get the almanac flag (whether we want any almanac data) from config params
	AlmanacFlag = getNum("GSM.RRLP.ALMANAC.ASSIST.PRESENT"),
	case AlmanacFlag of
		0 ->
			% if 0, then return empty list
			[];
		1 ->
			% otherwise read the almanac and generate the apdus
			Almanac = readAlmanac(),
			genAlmanacStuff(Almanac)
	end.

% recursive function that breaks list of satellites into groups of 8 (while actually generating the apdu)
genAlmanacStuff({_,[]}) -> [];
genAlmanacStuff({Global,Sats}) ->
	[almanacSubset({Global,Sats}, 1, 8) | genAlmanacStuff({Global, safeNthTail(8, Sats)})].

% lists:nthtail fails if you go path the end of a list.
% this function returns [] if you go past the end
safeNthTail(N, List) when length(List) =< N -> [];
safeNthTail(N, List) -> lists:nthtail(N, List).

% generate an apdu that contains reference location and utc and ionospheric info
smallEphemerisStuff(Ephemeris) ->
	#'PDU'{referenceNumber=1,
		component={assistanceData, #'AssistanceData'{
			    'gps-AssistData'=#'GPS-AssistData'{
					controlHeader=#'ControlHeader'{
						% reference time is moved to the location request
						% referenceTime=referenceTime(Ephemeris),
						refLocation=refLocation(),
						ionosphericModel=ionosphericModel(Ephemeris),
						utcModel=utcModel(Ephemeris)
					}}}}}.

% generate the apdu for Length satellites in the ephmeris, starting with First
navigationModelSubset(Ephemeris, First, Length) ->
	#'PDU'{referenceNumber=1,
		component={assistanceData, #'AssistanceData'{
			    'gps-AssistData'=#'GPS-AssistData'{
					controlHeader=#'ControlHeader'{
						navigationModel=navigationModel(Ephemeris, First, Length)
					}}}}}.

% generate the apdu for Length satellites in the almanac, starting with First
almanacSubset(Almanac, First, Length) ->
	{Week, Elements} = Almanac,
	ElementsSubset = lists:sublist(Elements, First, Length),
	#'PDU'{referenceNumber=1,
		component={assistanceData, #'AssistanceData'{
			    'gps-AssistData'=#'GPS-AssistData'{
					controlHeader=#'ControlHeader'{
						almanac=#'Almanac'{alamanacWNa=Week, almanacList=ElementsSubset}
					}}}}}.

% generate apdu for reference time
referenceTime(Ephemeris) ->
	% current utc
	{Today,Time} = erlang:universaltime(),
	% break out pieces of it
	{Year, Month, DayOfMonth} = Today,
	{Hours, Minutes, Seconds} = Time,
	% what nice, handy functions!
	DayOfWeek7 = calendar:day_of_the_week(Today),
	% day_of_the_week returns 7 for Sunday, but we need it to be 0
	DayOfWeek = DayOfWeek7 rem 7,
	% utc seconds into the week
	UtcSow = ((DayOfWeek * 24 + Hours) * 60 + Minutes) * 60 + Seconds,
	% get a day number for Jan 6, 1980 (when gps time started)
	DaysToWeek0 = calendar:date_to_gregorian_days(1980,1,6),
	% get a day number for the beginning of this week
	DaysToLastSunday = calendar:date_to_gregorian_days(Year,Month,DayOfMonth) - DayOfWeek,
	% what is the utc week number for this week
	UtcWeek = round((DaysToLastSunday - DaysToWeek0) / 7),
	% get leap second difference
	{GlobalAst, _} = Ephemeris,
	UtcDeltaTls = dictFetch("utcDeltaTls", GlobalAst),
	% convert utc to gps time
	Diff = UtcSow - UtcDeltaTls,
	case Diff < 0 of
		true ->
			GpsSow = Diff + 604800,
			GpsWeekFull = UtcWeek - 1;
		false ->
			GpsSow = Diff,
			GpsWeekFull = UtcWeek
	end,
	% encode the gps time for the apdu
	GPSTOW23b = roundAndCheck("GPSTOW23b", 0, GpsSow/0.08, 0, 7559999),
	GpsWeek = GpsWeekFull rem 1024,
	% generate the apdu
	#'ReferenceTime'{
		gpsTime = #'GPSTime'{
			gpsTOW23b = GPSTOW23b,
			gpsWeek = GpsWeek
		}
	}.

% generate the apdu for reference location
refLocation() -> 
	% get latitude, longitude, and altitude from config params
	Lat = getNum("GSM.RRLP.SEED.LATITUDE"),
	Long = getNum("GSM.RRLP.SEED.LONGITUDE"),
	Altitude = round(getNum("GSM.RRLP.SEED.ALTITUDE")),
	% generate the apdu
	% see 23.032, sect 7.3.6
	EllipsoidPointWithAltitudeAndUncertaintyEllipsoid = [144] ++ encodeLatitude(Lat) ++ encodeLongitude(Long) ++ encodeOctets(Altitude, 2, false) ++ [7,7,0,7,0],
	#'RefLocation'{threeDLocation = EllipsoidPointWithAltitudeAndUncertaintyEllipsoid}.

% generate navigation model apdu from Length satellite asts, starting at First
navigationModel({_, SatAsts}, First, Length) ->
	% get the satellite asts we want
	SatAstsOfInterest = lists:sublist(SatAsts, First, Length),
	% turn them into apdu elements
	SeqOfNavModelElement = [seqOfNavModelElement(SatAst) || SatAst <- SatAstsOfInterest],
	% generate the apdu
	#'NavigationModel'{navModelList=SeqOfNavModelElement}.

% given a dictionary of satellite ephemeris parameters, generate apdu encoding for navigation model
seqOfNavModelElement(SatAst) ->
	#'NavModelElement'{
		satelliteID = dictFetch("satelliteID", SatAst),
		satStatus = {newSatelliteAndModelUC, uncompressedEphemeris(SatAst)}}.

% fetch the parameters from the dictionary, and generate a big old apdu record
uncompressedEphemeris(SatAst) ->
	#'UncompressedEphemeris'{
		ephemCodeOnL2 = dictFetch("ephemCodeOnL2", SatAst),
		ephemURA = dictFetch("ephemURA", SatAst),
		ephemSVhealth = dictFetch("ephemSVhealth", SatAst),
		ephemIODC = dictFetch("ephemIODC", SatAst),
		ephemL2Pflag = dictFetch("ephemL2Pflag", SatAst),
		ephemSF1Rsvd = #'EphemerisSubframe1Reserved'{
		    reserved1 = 0,
			reserved2 = 0,
			reserved3 = 0,
			reserved4 = 0},
		ephemTgd = dictFetch("ephemTgd", SatAst),
		ephemToc = dictFetch("ephemToc", SatAst),
		ephemAF2 = dictFetch("ephemAF2", SatAst),
		ephemAF1 = dictFetch("ephemAF1", SatAst),
		ephemAF0 = dictFetch("ephemAF0", SatAst),
		ephemCrs = dictFetch("ephemCrs", SatAst),
		ephemDeltaN = dictFetch("ephemDeltaN", SatAst),
		ephemM0 = dictFetch("ephemM0", SatAst),
		ephemCuc = dictFetch("ephemCuc", SatAst),
		ephemE = dictFetch("ephemE", SatAst),
		ephemCus = dictFetch("ephemCus", SatAst),
		ephemAPowerHalf = dictFetch("ephemAPowerHalf", SatAst),
		ephemToe = dictFetch("ephemToe", SatAst),
		ephemFitFlag = dictFetch("ephemFitFlag", SatAst),
		ephemAODA = 0, % dictFetch("ephemAODA", SatAst), % TODO - can't find it
		ephemCic = dictFetch("ephemCic", SatAst),
		ephemOmegaA0 = dictFetch("ephemOmegaA0", SatAst),
		ephemCis = dictFetch("ephemCis", SatAst),
		ephemI0 = dictFetch("ephemI0", SatAst),
		ephemCrc = dictFetch("ephemCrc", SatAst),
		ephemW = dictFetch("ephemW", SatAst),
		ephemOmegaADot = dictFetch("ephemOmegaADot", SatAst),
		ephemIDot = dictFetch("ephemIDot", SatAst)}.

% same as above for ionospheric model
ionosphericModel({GlobalAst, _}) ->
	#'IonosphericModel'{
		alfa0 = dictFetch("alfa0", GlobalAst),
		alfa1 = dictFetch("alfa1", GlobalAst),
		alfa2 = dictFetch("alfa2", GlobalAst),
		alfa3 = dictFetch("alfa3", GlobalAst),
		beta0 = dictFetch("beta0", GlobalAst),
		beta1 = dictFetch("beta1", GlobalAst),
		beta2 = dictFetch("beta2", GlobalAst),
		beta3 = dictFetch("beta3", GlobalAst)}.

% same as above for utc model
utcModel({GlobalAst, _}) ->
	#'UTCModel'{
		utcA1 = dictFetch("utcA1", GlobalAst),
		utcA0 = dictFetch("utcA0", GlobalAst),
		utcTot = dictFetch("utcTot", GlobalAst),
		utcWNt = dictFetch("utcWNt", GlobalAst),
		utcDeltaTls = dictFetch("utcDeltaTls", GlobalAst),
		utcWNlsf = dictFetch("utcWNlsf", GlobalAst),
		utcDN = dictFetch("utcDN", GlobalAst),
		utcDeltaTlsf = dictFetch("utcDeltaTlsf", GlobalAst)}.

% get almanac or ephemeris file from Url and cache it in Filename.
% cache for up to MaxAgeInHours.
% TODO - It might be better to use one of erlang's own (indirect) locking mechanisms, but
% that would require deciding whether to use some persistent store (mnesia?) or a daemon
% that updates and serves this info.  And coding it.
% [I used to think it would be better to cache the encoded APDUs instead of the files, but
% now I think that would not be good, because the APDUs have time information that goes bad
% immediately, and satellite ordering which can also go bad immediately.]
% Gotta lock before checking age of file, because someone else could lock, replace the file,
% and unlock between when you check age and try to lock.  So end up locking just to read.
% Not optimal.
getFile(FileName, Url, MaxAgeInHours) ->
	% attempt a lock
	LockFile = FileName ++ ".lock",
	R = os:cmd("lockfile -r0 " ++ LockFile),
	% check if the output of lockfile command has "Sorry" in it
	T = string:str(R, "Sorry"),
	if
		T == 0 ->
			% "Sorry" not in output
			% file was not locked - read (or update and read), and remove lock
			F = getFile1(FileName, Url, MaxAgeInHours),
			os:cmd("rm -f " ++ LockFile),
			F;
		true ->
			% "Sorry" IS in output
			% file was locked - wait a sec and try again
			io:format("waiting\n"),
			timer:sleep(1000),
			getFile(FileName, Url, MaxAgeInHours)
	end.

% if Filename is younger than MaxAgeInHours, return contents of Filename
% if older, refresh from Url, then return contents
getFile1(FileName, Url, MaxAgeInHours) ->
	% get age of file
	MaxAgeInSeconds = round(60*60*MaxAgeInHours),
	Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
	case file:read_file_info(FileName) of
		% if we can't get file info, it's probably not there.  assume file is Very Very Old
		{error, _} -> ModTime = 0;
		% otherwise get the last modified time of the file
		{ok, FileInfo} -> ModTime = calendar:datetime_to_gregorian_seconds(FileInfo#'file_info'.mtime)
	end,
	% calculate the age of the file
	AgeInSeconds = Now - ModTime,
	io:format("info=~p is ~.1f hours old\n", [FileName, AgeInSeconds/3600]),
	% check if it's too old
	if
		AgeInSeconds > MaxAgeInSeconds ->
			% it's too old - update
			QuotedUrl = string:join(["'", Url, "'"], ""),
			Cmd = string:join(["wget -q -O", FileName, QuotedUrl], " "),
			io:format("info=~p\n", [Cmd]),
			os:cmd(Cmd);
		true ->
			% it's not too old - do nothing
			nop
	end,
	% and now read
	case file:open(FileName, [read]) of
		{error, Reason} -> io:format("error=~p\n", [Reason]), [];
		{ok, Device} -> get_all_lines(Device, [])
	end.

% read and return all the lines from Device
get_all_lines(Device, Accum) ->
	% get_line reads one line
	case io:get_line(Device, "") of
		% if eof, then close Device, reverse the list of lines, and return it
		eof  -> file:close(Device), lists:reverse(Accum);
		% otherwise clean the line and recursively build a (backwards, for efficiency) list of lines
		Line -> get_all_lines(Device, [cleanLine(Line)|Accum])
	end.

% read the almanac file, and parse it into a dictionary of global data and satellite fields
% also massage the data (unit conversions, scaling, etc) to go into the apdu
readAlmanac() ->
	% get the parse table
	Table = parseTable(),
	% read the file
	% the url and max cache time are config params
	Lines = getFile("/var/run/rrlp/almanac", getStr("GSM.RRLP.ALMANAC.URL"), getNum("GSM.RRLP.ALMANAC.REFRESH.TIME")),
	% io:format("Lines=~p\n", [Lines]),
	% Apply the parser to the lines.
	parseAlmanac([], Lines, [], Table, 0).

% parseAlmanac reads a line a calls itself, building a list of fields culled form the almanac,
% periodically turning that list into a dictionary and building a list of dictionaries, which
% eventually becomes the almanac apdu.  And the final wart is the single piece of global data,
% the week, which, when found, gets passed along as a separate argument to parseAlmanac.
% 1st arg: Ast = the list of (label,value) pairs for a satellite
% 2nd arg: Lines = lines remaining from almanac
% 3rd arg: AlmanacSoFar = list of apdu elements, one for each satellite
% 4th arg: Table = the parse table
% 5th arg: Week = which week is this
%
% when there are no more lines, return a tuple with the week (global) and almanac (satellite list)
parseAlmanac(_, [], AlmanacSoFar, _, Week) -> {Week, AlmanacSoFar};
% otherwise...
parseAlmanac(Ast, Lines, AlmanacSoFar, Table, Week) ->
	% peel off next line
	[NextLine|RestOfLines] = Lines,
	% io:format("line=~s\n", [NextLine]),
	% get the first and last fields from the line 
	case parseNextAlmanacLine(NextLine) of
		% ignore the lines with asterisks in the first field
		{"********",_} -> parseAlmanac(Ast, RestOfLines, AlmanacSoFar, Table, Week);
		% save week for global alamanacWNa
		{"week:",SetWeek} -> parseAlmanac(Ast, RestOfLines, AlmanacSoFar, Table, setWeek(SetWeek));
		% blank line: convert ast to element and add to almanac
		{"",_} -> parseAlmanac([], RestOfLines, [astToElement(Ast) | AlmanacSoFar], Table, Week);
		% otherwise first field is index into parse table, and last field is value
		{FirstField,LastField} ->
			% get encoding info from parse table
			{ok, [Label, Correction, Scale, Min, Max]} = dict:find(FirstField, Table),
			% encode the value
			AstEntry = {Label,encodeAndCheck(Label, LastField, Correction, Scale, Min, Max)},
			% add {label,encoded value} to Ast
			parseAlmanac([AstEntry|Ast], RestOfLines, AlmanacSoFar, Table, Week)
	end.

% I really want to replace this with to_num, but I'm documenting now, and if
% you change too much code while documenting you end up breaking things.
setWeek(Week) ->
	case string:to_integer(Week) of
		{error, _} -> 0;
		{Num, _} -> Num rem 256
	end.

% return a parse table that tells what almanac fields go where, and how to encode them for the apdu
parseTable() -> dict:from_list ([
% name of field in almanac file
% name in ASN records, correction type, scaling (exponent of 2), min after scaling, max after scaling
% The post-scaling values are what go into the ASN elements.
		{"ID:",				["satelliteID",			4,   0,        0,       63]},

		{"Eccentricity:",	["almanacE",			0, -21,        0,    65535]},
		{"Time",			["alamanacToa",			0,  12,        0,      255]},
		{"Orbital",			["almanacKsii",			2, -19,   -32768,    32767]},
		{"Rate",			["almanacOmegaDot",		1, -38,   -32768,    32767]},

		{"Health:",			["almanacSVhealth",		0,   0,        0,      255]},

		{"SQRT(A)",			["almanacAPowerHalf",	0, -11,        0, 16777215]},
		{"Right",			["almanacOmega0",		1, -23, -8388608,  8388607]},
		{"Argument",		["almanacW",			1, -23, -8388608,  8388607]},
		{"Mean",			["almanacM0",			1, -23, -8388608,  8388607]},
		{"Af0(s):",			["almanacAF0",			0, -20,    -1024,     1023]},
		{"Af1(s/s):",		["almanacAF1",			0, -38,    -1024,     1023]}]).

% turn a list of {label,value} for a satellite tuples into an apdu almanac element
astToElement(Ast) ->
	Dict = dict:from_list(Ast),
	#'AlmanacElement'{
		satelliteID = dictFetch("satelliteID", Dict),
		almanacE = dictFetch("almanacE", Dict),
		alamanacToa = dictFetch("alamanacToa", Dict),
		almanacKsii = dictFetch("almanacKsii", Dict),
		almanacOmegaDot = dictFetch("almanacOmegaDot", Dict), 
		almanacSVhealth = dictFetch("almanacSVhealth", Dict), 
		almanacAPowerHalf = dictFetch("almanacAPowerHalf", Dict),
		almanacOmega0 = dictFetch("almanacOmega0", Dict),
		almanacW = dictFetch("almanacW", Dict),
		almanacM0 = dictFetch("almanacM0", Dict),
		almanacAF0 = dictFetch("almanacAF0", Dict),
		almanacAF1 = dictFetch("almanacAF1", Dict)}.

% return the first and last field of an almanac line
parseNextAlmanacLine(Line) ->
	% tokenize the line
	Fields = string:tokens(Line, " "),
	case length(Fields) of
		0 ->
			% blank line
			{"",undefined};
		_ ->
			% otherwise return first and last fields
			FirstField = lists:nth(1, Fields),
			LastField = lists:last(Fields),
			{FirstField, LastField}
	end.

% convert string representation of number into a number
to_num(String) ->
	Zeroed = case String of
		% to_float doesn't like numbers that start with decimal point, so prepend 0
		[$. | Rest] -> [$0, $. | Rest];
		% similarly, -.digits -> -0.digits
		[$-, $. | Rest] -> [$-, $0, $. | Rest];
		% otherwise do nothing
		_ -> String
	end,
	{Converted,_} = case string:to_float(Zeroed) of
		% if to_float fails, then do to_integer
		{error,_} -> string:to_integer(Zeroed);
		% otherwise return the float number
		Y -> Y
	end,
	Converted.

% The asn code does the range check too, but doesn't give good feedback.
% Plus when it's live we don't want to crash.
% Instead, we issue a warning, make the value legal, and keep going.
% Label = apdu element name, plus we use it for error message
% Value = the value
% Correction = code that tells what sort of unit conversion is needed
% Scale = scaling needed for apdu conversion
% Min, Max = range of legal values
encodeAndCheck(Label, Value, Correction, Scale, Min, Max) ->
	% convert string to number
	Converted = to_num(Value),
	% apply correction
	Corrected = case Correction of
			% 0 => none
			0 -> Converted;
			% 1 => semicircles to radians
			1 -> Converted / math:pi();
			% 2 => semicircles to radians plus bias (for inclination)
			2 -> Converted / math:pi() - 0.3;
			% 3 => modulo 256
			3 -> Converted rem 256;
			% 4 => subtract one (for satellite number)
			4 -> Converted - 1
		end,
	% apply the scaling
	Scaled = Corrected / math:pow(2, Scale),
	% turn into an integer, and check that its in the legal range
	roundAndCheck(Label, Converted, Scaled, Min, Max).

% make sure value is in the range [min..max]
% if it isn't, issure warning and make it legal
roundAndCheck(Label, Converted, Value, Min, Max) ->
	Rounded = round(Value),
	case Rounded < Min of
		true ->
			rangeError(Label, Converted, Rounded, Min, Max),
			Min;
		false ->
			case Rounded > Max of
				true ->
					rangeError(Label, Converted, Rounded, Min, Max),
					Max;
				false ->
					Rounded
			end
	end.

% the out-of-range warning
rangeError(Label, Converted, Rounded, Min, Max) ->
	io:format("range error=~s of ~p (~p) doesn't fit in (~p,~p)\n", [Label, Converted, Rounded, Min, Max]).

% read the ephemeris, return a dictionary of global params, and a list of dictionaries of satellite parameters
% the list is sorted, closest (to reference location) first
readEphemeris() ->
	% read the adjustment table that gives corrections, scaling, etc
	AdjustTable = ephemerisAdjustTable(),
	% read the file.  the url and max cache age are config params
	Lines = getFile("/var/run/rrlp/ephemeris", getStr("GSM.RRLP.EPHEMERIS.URL"), getNum("GSM.RRLP.EPHEMERIS.REFRESH.TIME")),
	% io:format("Lines=~p\n", [Lines]),
	% parse the lines from the global part of the ephemeris
	% RestOfLines = the satellite part of the ephemeris
	% GlobalAst = list of {label, value} tuples form the global part of the ephemeris
	{RestOfLines, GlobalAst} = parseGlobalEphemeris(Lines, AdjustTable, []),
	% turn global ast into global dictionary
	GlobalAstDict = dict:from_list(GlobalAst),
	% parse the lines from the non-global part of the ephemeris
	% SatAsts = list of {label, value} tuples form the non-global part of the ephemeris
	SatAsts = parseSatEphemeris(RestOfLines, AdjustTable, []),
	% turn SatAsts into list of dictionaries
	SatAstDicts = [dict:from_list(SatAst) || SatAst <- SatAsts],
	% turn SatAsts into list of {dictionary,angle} where angle = the angle between ref loc and the satellite (wrt the center of the earth)
	SatAstDictsAndAngles = [{SatAst,angleBetween(SatAst)} || SatAst <- SatAstDicts],
	% sort them, smallest angle first
	SortedDicts = mySort(SatAstDictsAndAngles),
	% return the global dictionary and list of sorted satellite dictionaries
	{GlobalAstDict, SortedDicts}.


% For [{Thing,Value},...], mySort will sort by Value, then return [Thing,...]
% In other words, if you have a list of things you want sorted, turn them into a
% list of tuples, and in each tuple you put the thing and the value associated with it,
% and this will sort so that min value comes first, then throw out the values, leaving
% just the things.
mySort(Unsorted) ->
	% perform the sort
	Sorted = lists:sort(fun mySorter/2, Unsorted),
	% throw out the values, leaving just the things
	[X || {X,_} <- Sorted].
% sort function, which extracts the values of two items, and returns their comparison.
mySorter({_,A}, {_,B}) -> A =< B.

% return the angle between a satellite (given its ast) and our reference location (earth center = vertex)
angleBetween(SatAst) ->
	% locate satellite in cartesian coordinates
	XYZ = ast2xyz(SatAst),
	% convert to latitude and longitude
	{SatLat,SatLong} = xyz2latlong(XYZ),
	% get ref loc from config params, and convert to radians
	MobLat = deg2rad(getNum("GSM.RRLP.SEED.LATITUDE")),
	MobLong = deg2rad(getNum("GSM.RRLP.SEED.LONGITUDE")),
	% use spherical law of cosines to get the cosine of the angle between the satellite and ref loc
	CosAngleBetween = math:sin(MobLat) * math:sin(SatLat) +
		math:cos(MobLat) * math:cos(SatLat) * math:cos(MobLong - SatLong),
	% convert to angle
	AngleBetween = math:acos(CosAngleBetween),
	% and return it
	AngleBetween.

% convert between radians and degrees
deg2rad(Deg) -> Deg * math:pi() / 180.
% rad2deg(Rad) -> Rad * 180 / math:pi().

% replace all whitespace and invisible characters with a space
cleanLine(Line) ->
	% this applies blank/1 to each character of the line
	[blank(X) || X <- Line].
blank(Char) ->
	case Char < $  of
		% if value of char is less than that of blank, then return blank
		true -> $ ;
		% otherwise no change
		false -> Char
	end.

% parse the global part of the ephemeris
% return the rest of the lines of the ephemeris, and a list of {label,value} tuples
% [NextLine|RestOfLines] = remaining lines of ephemeris
% AdjustTable = adjustment table for ephemeris
% GlobalAst = list of {label,value} tuples so far
parseGlobalEphemeris([NextLine|RestOfLines], AdjustTable, GlobalAst) ->
	% tokenize the line
	Tokens = string:tokens(NextLine, " "),
	% we can tell what kind of line it is from the last field in the line
	case lists:last(Tokens) of
		"ALPHA" ->
			% the first four fields are alfa[0..3]
			% globalStuff puts the fields into the global ast, then recurses
			globalStuff(["alfa0", "alfa1", "alfa2", "alfa3"], Tokens, RestOfLines, AdjustTable, GlobalAst);
		"BETA" ->
			% the first four fields are beta[0..3]
			globalStuff(["beta0", "beta1", "beta2", "beta3"], Tokens, RestOfLines, AdjustTable, GlobalAst);
		"A0,A1,T,W" ->
			% the first four fields are utc parameters
			globalStuff(["utcA0", "utcA1", "utcTot", "utcWNt"], Tokens, RestOfLines, AdjustTable, GlobalAst);
		"SECONDS" ->
			% the first field is leap seconds
			globalStuff(["utcDeltaTls"], Tokens, RestOfLines, AdjustTable, GlobalAst);
		"HEADER" ->
			% 
			% I didn't find future leap second info in the ephemeris,
			% so I'm setting utcDeltaTlsf to zero, then week and day don't matter
			% TODO - is this ok?
			Eles = [{"utcWNlsf", 0}, {"utcDN", 0}, {"utcDeltaTlsf", 0}],
			% add these labels and values to the global ast
			NewGlobalAst = Eles ++ GlobalAst,
			% return rest of lines (satellite part of ephemeris) and the global ast we've built
			{RestOfLines, NewGlobalAst};
		_ -> 
			% ignore lines that don't match one of the above
			parseGlobalEphemeris(RestOfLines, AdjustTable, GlobalAst)
	end.

% given a list of labels, associate them with the first N tokens, apply adjustments, add to global ast, and recurse
globalStuff(Labels, Tokens, RestOfLines, AdjustTable, GlobalAst) ->
	% make a list of labels and their positions
	LabelsPos = lists:zip(Labels, lists:seq(1,length(Labels))),
	% make list of {label,adjusted value} tuples
	% oops, I should have zipped the labels and tokens, eliminating lists:nth
	Eles = [stuff(Label, lists:nth(Pos, Tokens), AdjustTable) || {Label,Pos} <- LabelsPos],
	% add the elements to the global ast
	NewGlobalAst = Eles ++ GlobalAst,
	% recurse
	parseGlobalEphemeris(RestOfLines, AdjustTable, NewGlobalAst).

% parse 8 lines at a time from the ephemeris.
% the 8 lines have all the parameters for a satellite.
% get them, adjust them, put them into a list of {label,value} tuples, and add that list to Asts
% Lines = reset of lines of ephemeris
% AdjustTable = table that tells how to adjust the fields of the ephemeris
% Asts = list of satellites; each satellite is a list of {label,value} tuples
% 
% no more lines.  return Asts
parseSatEphemeris([], _, Asts) -> Asts;
% otherwise...
parseSatEphemeris(Lines, AdjustTable, Asts) ->
	% tokenize each of the next 8 lines
	Tokens = [string:tokens(lists:nth(N, Lines), " ") || N <- lists:seq(1,8)],
	% for each satellite parameter, pick it out of the lists of tokens, adjust it, and put it into a list
	NewAst = [
		stuff("satelliteID", nmth(1,1,Tokens), AdjustTable),
		stuff("ephemToc", epoch(Tokens), AdjustTable),
		stuff("ephemAF0", nmth(1,8,Tokens), AdjustTable),
		stuff("ephemAF1", nmth(1,9,Tokens), AdjustTable),
		stuff("ephemAF2", nmth(1,10,Tokens), AdjustTable),

		% stuff("ephemIODE", nmth(2,1,Tokens), AdjustTable),
		stuff("ephemCrs", nmth(2,2,Tokens), AdjustTable),
		stuff("ephemDeltaN", nmth(2,3,Tokens), AdjustTable),
		stuff("ephemM0", nmth(2,4,Tokens), AdjustTable),

		stuff("ephemCuc", nmth(3,1,Tokens), AdjustTable),
		stuff("ephemE", nmth(3,2,Tokens), AdjustTable),
		stuff("ephemCus", nmth(3,3,Tokens), AdjustTable),
		stuff("ephemAPowerHalf", nmth(3,4,Tokens), AdjustTable),

		stuff("ephemToe", nmth(4,1,Tokens), AdjustTable),
		stuff("ephemCic", nmth(4,2,Tokens), AdjustTable),
		stuff("ephemOmegaA0", nmth(4,3,Tokens), AdjustTable),
		stuff("ephemCis", nmth(4,4,Tokens), AdjustTable),

		stuff("ephemI0", nmth(5,1,Tokens), AdjustTable),
		stuff("ephemCrc", nmth(5,2,Tokens), AdjustTable),
		stuff("ephemW", nmth(5,3,Tokens), AdjustTable),
		stuff("ephemOmegaADot", nmth(5,4,Tokens), AdjustTable),

		stuff("ephemIDot", nmth(6,1,Tokens), AdjustTable),
		stuff("ephemCodeOnL2", nmth(6,2,Tokens), AdjustTable),
		% stuff("ephemWeek", nmth(6,3,Tokens), AdjustTable),
		stuff("ephemL2Pflag", nmth(6,4,Tokens), AdjustTable),

		stuff("ephemURA", nmth(7,1,Tokens), AdjustTable),
		stuff("ephemSVhealth", nmth(7,2,Tokens), AdjustTable),
		stuff("ephemTgd", nmth(7,3,Tokens), AdjustTable),
		stuff("ephemIODC", nmth(7,4,Tokens), AdjustTable),

		stuff("ephemFitFlag", nmth(8,2,Tokens), AdjustTable)],

	% add the list of {label,value} tuples to Asts and recurse, skipping the 8 lines we just did
	parseSatEphemeris(lists:nthtail(8, Lines), AdjustTable, [NewAst|Asts]).

% calculate the epoch from 6 tokens
% epoch = how many 1.5 seconds since the beginning of the current week
% epoch = reference time for this satellite's ephemeris data
epoch(Tokens) ->
	Year = 2000 + to_num(nmth(1,2,Tokens)),
	Month = to_num(nmth(1,3,Tokens)),
	Day = to_num(nmth(1,4,Tokens)),
	Hours = to_num(nmth(1,5,Tokens)),
	Minutes = to_num(nmth(1,6,Tokens)),
	Seconds = to_num(nmth(1,7,Tokens)),
	Dow7 = calendar:day_of_the_week({Year,Month,Day}),
	Dow = Dow7 rem 7, % day_of_the_week returns 7 for Sunday instead of 0
	EpochSeconds = ((Dow * 24 + Hours) * 60 + Minutes) * 60 + Seconds,
	EpochUnits = EpochSeconds / 1.5,
	% back to string for scaling
	integer_to_list(round(EpochUnits)).

% given label, value, and adjustment table, do the adjustment, check range, and return {label,adjusted value}
stuff(Label, Value, AdjustTable) ->
	% get adjustment info from adjustment table
	[Correction, Scale, Min, Max] = dictFetch(Label, AdjustTable),
	% adjust 
	Adjusted = encodeAndCheck(Label, Value, Correction, Scale, Min, Max),
	% return tuple
	{Label, Adjusted}.

% find the value associate with key in a dictionary
dictFetch(Key, Dict) ->
	case dict:find(Key, Dict) of
		{ok,Value} ->
			% return value on successful lookup
			Value;
		error ->
			% yell and return 0 on unsuccessful lookup
			io:format("~p not found in dict\n", [Key]),
			0
	end.

% given a list of lists of tokens, the the Mth token in the Nth list
% as used above, N is the satellite data line, and M is the field of that line
nmth(N, M, Tokens) ->
	lists:nth(M, lists:nth(N, Tokens)).

% return the adjust table for the ephemeris
% it has, for each label of the apdu, adjustments and range check for that label
ephemerisAdjustTable() -> dict:from_list ([
% See comments on parseTable().  This is similar, but there are no search strings on the input lines.
		{"satelliteID",		[4,   0,           0,         63]},
		% IonosphericModel
		{"alfa0",			[0, -30,        -128,        127]},
		{"alfa1",			[0, -27,        -128,        127]},
		{"alfa2",			[0, -24,        -128,        127]},
		{"alfa3",			[0, -24,        -128,        127]},
		{"beta0",			[0,  11,        -128,        127]},
		{"beta1",			[0,  14,        -128,        127]},
		{"beta2",			[0,  16,        -128,        127]},
		{"beta3",			[0,  16,        -128,        127]},

		% UTCModel
		{"utcA1",			[0, -30,    -8388608,    8388607]},
		{"utcA0",			[0, -50, -2147483648, 2147483647]},
		{"utcTot",			[0,  12,           0,        255]},
		{"utcWNt",			[3,   0,           0,        255]}, % TODO - is correction correct?
		{"utcDeltaTls",		[0,   0,        -128,        127]},
		{"utcWNlsf",		[0,   0,           0,        255]},
		{"utcDN",			[0,   0,        -128,        127]},
		{"utcDeltaTlsf",	[0,   0,        -128,        127]},

		% UncompressedEphemeris
		{"ephemCodeOnL2",   [0,   0,           0,          3]},
		{"ephemURA",        [0,   0,           0,         15]},
		{"ephemSVhealth",   [0,   0,           0,         63]},
		{"ephemIODC",       [0,   0,           0,       1023]},
		{"ephemL2Pflag",    [0,   0,           0,          1]},
		% ephemSF1Rsvd   gets a list.
		{"ephemTgd",        [0, -31,        -128,        127]},
		{"ephemToc",        [0,   4,           0,      37799]},
		{"ephemAF2",        [0, -55,        -128,        127]},
		{"ephemAF1",        [0, -43,      -32768,      32767]},
		{"ephemAF0",        [0, -31,    -2097152,    2097151]},
		{"ephemCrs",        [0,  -5,      -32768,      32767]},
		{"ephemDeltaN",     [1, -43,      -32768,      32767]},
		{"ephemM0",         [1, -31, -2147483648, 2147483647]},
		{"ephemCuc",        [0, -29,      -32768,      32767]},
		{"ephemE",          [0, -33,           0, 4294967295]},
		{"ephemCus",        [0, -29,      -32768,      32767]},
		{"ephemAPowerHalf", [0, -19,           0, 4294967295]},
		{"ephemToe",        [0,   4,           0,      37799]},
		{"ephemFitFlag",    [0,   0,           0,          1]},
		{"ephemAODA",       [0,   0,           0,         31]},
		{"ephemCic",        [0, -29,      -32768,      32767]},
		{"ephemOmegaA0",    [1, -31, -2147483648, 2147483647]},
		{"ephemCis",        [0, -29,      -32768,      32767]},
		{"ephemI0",         [1, -31, -2147483648, 2147483647]},
		{"ephemCrc",        [0,  -5,      -32768,      32767]},
		{"ephemW",          [1, -31, -2147483648, 2147483647]},
		{"ephemOmegaADot",  [1, -43,    -8388608,    8388607]},
		{"ephemIDot",       [1, -43,       -8192,       8191]}]).

% most of the equations below are from "Server-Side GPS and Assisted-GPS in Java" by Neil Harper.
% except for the bug I removed from Vk

% convert cartesian coordinates to latitude and longitude
xyz2latlong({X, Y, Z}) ->

	% The semi-major axis of the ellipsoid as defined by WGS 84.
	A_SEMI_MAJOR = 6378137.0,

	% The semi-minor axis of the ellipsoid as defined by WGS 84. */
	B_SEMI_MINOR = 6356752.3142,

	% The flattening as defined by WGS 84. */
	FLATTENING = 1 / 298.257223563,

	% The eccentricity squared derived from flatenning WGS 84. */
	E_SQUARED = (2 * FLATTENING) - (FLATTENING * FLATTENING),

	% The second eccentricity squared derived WGS 84. */
	SECOND_ECCENTRICITY_SQUARED = ((A_SEMI_MAJOR * A_SEMI_MAJOR) - (B_SEMI_MINOR * B_SEMI_MINOR)) / (B_SEMI_MINOR * B_SEMI_MINOR),

	case ((X == 0) and (Y == 0)) of
		true ->
			% one of the poles
			Long = 0,
			case Z > 0 of
				true ->
					% north pole
					Lat = math:pi()/2;
				false ->
					% south pole
					Lat = -math:pi()/2
			end;
		false ->
			% not one of the poles
			P = math:sqrt(X*X + Y*Y),
			Theta = math:atan((Z * A_SEMI_MAJOR) / (P * B_SEMI_MINOR)),
			Lat = math:atan(
				(Z + (SECOND_ECCENTRICITY_SQUARED * B_SEMI_MINOR * math:pow(math:sin(Theta), 3)))
				/
				(P - (E_SQUARED * A_SEMI_MAJOR * math:pow(math:cos(Theta), 3)))
				),
			Long = math:atan2(Y, X)
	end,
	{Lat, Long}.

% This is used to get satellite parameters out of the intermediate dictionary built for the ephemeris apdu.
% Some of the adjustments made for the apdu have to be undone before using the data for the satellite location calculations.
getAndUnMess(Label, SatAst, AdjustTable) ->
	% get the field out of the dictionary
	Scaled = dictFetch(Label, SatAst),
	% get the adjustment data
	[Corr, Scale, _, _] = dictFetch(Label, AdjustTable),
	% scaling was just to make the data fit into an integer.  Undo the scaling.
	UnScaled = Scaled * math:pow(2, Scale),
	% if the value was converted to semicircles, convert it back to radians
	case Corr of
		1 -> UnCorrected = UnScaled * math:pi();
		_ -> UnCorrected = UnScaled
	end,
	UnCorrected.

% get utc seconds of week to locate the satellite
utcSow() ->
	{Today,Time} = erlang:universaltime(),
	{Hours, Minutes, Seconds} = Time,
	DayOfWeek7 = calendar:day_of_the_week(Today),
	DayOfWeek = DayOfWeek7 rem 7, % day_of_the_week returns 7 for Sunday instead of 0
	UtcSow = ((DayOfWeek * 24 + Hours) * 60 + Minutes) * 60 + Seconds,
	UtcSow.

% given a dictionary of satellite parameters, calculate the satellite's location in cartesian coordinates
ast2xyz(Ast) ->

    MAX_NUM_ITERATIONS_CALCULATING_EK = 20,

	% The WGS84 value of the earths universal gravitation parameter in cubic metres per square second
	% (Mass of earths atmosphere included).
	WGS84_MU_EARTHS_GRAV_PARAMETER = 3.986005E+14,

    % The WGS84 value of the earths rotation rate in radians per second.
    OMEGA_EARTHS_ROTATION_RATE_RADIANS_SECOND = 7.2921151467E-05,

	AdjustTable = ephemerisAdjustTable(),

	E = getAndUnMess("ephemE", Ast, AdjustTable),
	SqrtA = getAndUnMess("ephemAPowerHalf", Ast, AdjustTable),
	DeltaN = getAndUnMess("ephemDeltaN", Ast, AdjustTable),
	M0 = getAndUnMess("ephemM0", Ast, AdjustTable),
	Omega = getAndUnMess("ephemW", Ast, AdjustTable),
	Cuc = getAndUnMess("ephemCuc", Ast, AdjustTable),
	Cus = getAndUnMess("ephemCus", Ast, AdjustTable),
	Crc = getAndUnMess("ephemCrc", Ast, AdjustTable),
	Crs = getAndUnMess("ephemCrs", Ast, AdjustTable),
	Cic = getAndUnMess("ephemCic", Ast, AdjustTable),
	Cis = getAndUnMess("ephemCis", Ast, AdjustTable),
	I0 = getAndUnMess("ephemI0", Ast, AdjustTable),
	Idot = getAndUnMess("ephemIDot", Ast, AdjustTable),
	Omega0 = getAndUnMess("ephemOmegaA0", Ast, AdjustTable),
	Omegadot = getAndUnMess("ephemOmegaADot", Ast, AdjustTable),
	Toe = getAndUnMess("ephemToe", Ast, AdjustTable),

	% Calculate A, n0 and n using the formulas from the GPS IS

	A = math:pow(SqrtA, 2),
	N0 = math:sqrt(WGS84_MU_EARTHS_GRAV_PARAMETER / math:pow(A, 3)),
	N = N0 + DeltaN,

	TimeOfWeekSeconds = utcSow(),
	Tk = calculateTimeFromEpoch(TimeOfWeekSeconds, Toe),
	Mk = M0 + N * Tk, % mean anomaly
	Ek = calculateEccentricAnomaly(Mk, MAX_NUM_ITERATIONS_CALCULATING_EK, E, Mk), % calcuate the eccentric anomaly

	% true anomaly
	Vk = math:atan2((math:sqrt(1 - math:pow(E, 2)) * math:sin(Ek)) / (1 - E * math:cos(Ek)),
			(math:cos(Ek) - E) / (1 - E * math:cos(Ek))),

	PhiK = Vk + Omega, % argument of latitude

	Duk = Cus * math:sin(2 * PhiK) + Cuc * math:cos(2 * PhiK), % argument of latitude correction
	Drk = Crc * math:cos(2 * PhiK) + Crs * math:sin(2 * PhiK), % radius correction
	Dik = Cic * math:cos(2 * PhiK) + Cis * math:sin(2 * PhiK), % correction to inclination

	Uk = PhiK + Duk, % corrected argument of latitude

	Rk = A * (1 - E * math:cos(Ek)) + Drk, % corrected radius
	Ik = I0 + Dik + (Idot * Tk), % corrected inclination

	Xk = Rk * math:cos(Uk), % position x in orbital plane
	Yk = Rk * math:sin(Uk), % position y in orbital plane

	% Corrected longitude of ascending node
	Omegak = Omega0 +
		((Omegadot - OMEGA_EARTHS_ROTATION_RATE_RADIANS_SECOND) * Tk) -
		OMEGA_EARTHS_ROTATION_RATE_RADIANS_SECOND * Toe,

	X = Xk * math:cos(Omegak) - Yk * math:cos(Ik) * math:sin(Omegak), % ECEF x
	Y = Xk * math:sin(Omegak) + Yk * math:cos(Ik) * math:cos(Omegak), % ECEF y
	Z = Yk * math:sin(Ik), % ECEF z

	{X,Y,Z}.


% This method determines Ek through iteration. Ek is the eccentric
% anomaly and is defined in the GPS SS and the GPS IS as one of the
% calculations used for calculating the location of the satellite.
% It normally converges in 5 to 8 iterations.
%
% no iterations left
calculateEccentricAnomaly(_, 0, _, Ek) ->
	io:format("note=calculateEccentricAnomaly did not converge\n"),
	Ek;
% otherwise
calculateEccentricAnomaly(Mk, IterationsLeft, E, Ek) ->
	NewEk = Mk + E * math:sin(Ek),
	case abs(Ek - NewEk) >= 1.0e-15 of
		true -> 
			calculateEccentricAnomaly(Mk, IterationsLeft - 1, E, NewEk);
		false ->
			NewEk
	end.

% given tow and toe, calculate time from epoch
calculateTimeFromEpoch(TimeOfWeekSeconds, Toe) ->
	Tk = TimeOfWeekSeconds - Toe, % time from ephemeris reference epoch
	case Tk > 302400 of
		true ->
			Tk - 604800;
		false ->
			case Tk < -302400 of
				true ->
					Tk + 604800;
				false ->
					Tk
			end
	end.

% test - list satellites and their positions
testpos() ->
	{_, Asts} = readEphemeris(),
	A = [testpos(Ast) || Ast <- Asts],
	B = mySort(A),
	io:format("~p\n", [B]).

% test - return location and id tuple for satellite, ready to sort
testpos(Ast) ->
	ID = dictFetch("satelliteID", Ast),
	XYZ = ast2xyz(Ast),
	LL = xyz2latlong(XYZ),
	{{ID, XYZ, LL}, ID}.
