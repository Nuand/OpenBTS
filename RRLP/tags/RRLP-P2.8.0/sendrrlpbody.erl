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


-module(sendrrlpbody).
-export([sendrrlp/2,main/1,requestByAccuracy/2]).

-include("RRLP.hrl").

% send a query with varying accuracy. Accuracy is not linear, it
% is in the range of 0-127 and corresponds to 1 meter up to 1800 km.
% 60->3km, 40->443m, 20->57.3m, 2->2.1m
% see 03.32 V7.2.0

main([IMSI, Accuracy, ReferenceNumber])
    when is_list(IMSI), is_list(Accuracy), is_list(ReferenceNumber) ->
    sendrrlp(IMSI, makeRRLPQuery(
        list_to_integer(Accuracy), list_to_integer(ReferenceNumber)));
main([IMSI, Accuracy, ReferenceNumber])
    when is_list(IMSI), is_integer(Accuracy), is_integer(ReferenceNumber) ->
    sendrrlp(IMSI, makeRRLPQuery(Accuracy, ReferenceNumber)).

makeRRLPQuery(Accuracy, ReferenceNumber) ->
    {ok, Binary} = xrrlp:encodePDU(requestByAccuracy(Accuracy, ReferenceNumber)),
    io:format("sending ~s~n", [util:bin_to_uhex_string(Binary)]),
    util:bin_to_uhex_string(Binary).

requestByAccuracy(Accuracy, ReferenceNumber) ->
    #'PDU'{referenceNumber=ReferenceNumber,
        component={msrPositionReq, #'MsrPosition-Req'{
            positionInstruct=#'PositionInstruct'{
                methodType={msBased, Accuracy},
                positionMethod=gps,
                measureResponseTime=1,
                useMultipleSets=oneSet
            }}}}.
    % The record syntax above is equivalent to the one below if Accuracy==60
    % (just to remember the conversion for now, being an erlang newb).
    %{'PDU',2, % reference number
    %    {msrPositionReq,
    %        {'MsrPosition-Req',
    %            {'PositionInstruct',{msBased,60},gps,2,oneSet,asn1_NOVALUE},
    %            asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,
    %            asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE,asn1_NOVALUE}}}

%% Send a packet to port 5062 - this is identicle to sendrrlp in CLI/CLI.cpp
%% RRLPQuery must be a hex string - that is, [40] == [16#28] is encoded as "28"
sendrrlp(IMSI, RRLPQuery) ->
    {ok, S} = gen_udp:open(0),
    {ok, Port} = inet:port(S),
    gen_udp:send(S, localhost, 5062, makeSIPPacketFromRRLP(IMSI, RRLPQuery, Port)),
    gen_udp:close(S).

% TODO: should I use a random call-id? I don't actually care for an answer
% since this is not a valid SIP anyhow.
makeSIPPacketFromRRLP(IMSI, RRLPQuery, Port) when is_list(IMSI), is_list(RRLPQuery) ->
    makeSIPPacket(IMSI, "RRLP" ++ RRLPQuery, Port).
    
makeSIPPacket(IMSI, Message, Port) ->
    Random = integer_to_list(random:uniform(1000)),
    "MESSAGE sip:IMSI" ++ IMSI ++ "@localhost SIP/2.0
Via: SIP/2.0/TCP localhost;branch=z9hG4bK776sgdkse
Max-Forwards: 2
From: RRLP@localhost:" ++ integer_to_list(Port) ++ ";tag=49583
To: sip:IMSI" ++ IMSI ++ "@localhost
Call-ID: " ++ Random ++ "@127.0.0.1:5063
CSeq: 1 MESSAGE
Content-Type: text/plain
Content-Length: " ++ integer_to_list(length(Message)) ++ "

" ++ Message ++ "\n".


