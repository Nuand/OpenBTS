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


-module(xrrlp).
-export([decode/1, decode_pos/1, decodePDU/1, encodePDU/1]).

%%% Module to decode RRLP messages. All the heavy work is done in RRLP.erl which is
%%% automatically generated from asn (see Makefile).
%%% The only decoding going on by hand here is that of the Position returned, following 3GPP TS 23.032.

decode(Data) ->
    case 'RRLP':decode('PDU', Data) of
        {error, Bla} -> {error, Bla};
        {ok, {'PDU', _referenceNumber, Component}} ->
            decodeComponent(Component)
    end.

decodeComponent(Component) ->
    case Component of
        {msrPositionRsp,MsrPositionRsp} -> decodeMsrPositionRsp(MsrPositionRsp);
        {assistanceDataAck,'NULL'} -> {assitanceDataAck};
        {msrPositionReq,{'MsrPosition-Req', PositionReq,_,_,_,_,_,_,_,_}} -> {msrPositionReq, PositionReq}
        %_ -> Component % catch all for debugging - remove!
    end.

decodeMsrPositionRsp({'MsrPosition-Rsp',_,_,_,{'LocationInfo',_Reference,_TOW,_FixType,PositionInfo},
     _,_,_,_,_,_}) -> decode_pos(list_to_binary(PositionInfo));
decodeMsrPositionRsp({'MsrPosition-Rsp',_,_,_,_,_,{'LocationError',gpsAssDataMissing,{'AdditionalAssistanceData',AdditionalAssistanceData,_,_}},_,_,_,_}) ->
        {additionalAssistanceData, AdditionalAssistanceData}.


% Decode the Position part from an RRLP Position Response.
% 7.3.5 Ellipsoid Point
decode_pos(<<8:4,_Spare:4,LatSign:1,Lat23:23,Lon24:24,AltDir:1,Alt15:15,_Rest/binary>>) ->
    io:format("Parsing 8~n", []),
    {(1 - LatSign*2)*Lat23*90/(1 bsl 23), Lon24*360/(1 bsl 24), (1-AltDir*2)*Alt15 } ;
% 7.3.6 Ellipsoid Point with altitude and uncertainty ellipsoid - we ignore the uncertainty
decode_pos(<<9:4,_Spare:4,LatSign:1,Lat23:23,Lon24:24,AltDir:1,Alt15:15,Rest/binary>>) ->
    io:format("Parsing 9 - Lat23 ~s Rest ~s~n", [util:bin_to_zeroones(<<Lat23:23>>),
        util:bin_to_zeroones(<<Rest/binary>>)]),
    {(1 - LatSign*2)*Lat23*90/(1 bsl 23), Lon24*360/(1 bsl 24), (1-AltDir*2)*Alt15 };
% 7.3.1 Ellipsoid Point or anything else really (catch all)
decode_pos(<<What:4,_Spare:4,LatSign:1,Lat23:23,Lon24:24,_Rest/binary>>) ->
    io:format("Parsing ~w~n", [What]),
    {(1 - LatSign*2)*Lat23*90/(1 bsl 23), Lon24*360/(1 bsl 24), 0.0}.

% Raw Encoding/Decoding
encodePDU(Data) ->
    'RRLP':encode('PDU', Data).

decodePDU(Data) ->
    'RRLP':decode('PDU', Data).

