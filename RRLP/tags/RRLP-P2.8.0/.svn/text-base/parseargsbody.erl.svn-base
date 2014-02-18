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


-module(parseargsbody).
-export([parsebin/1,main/1,parsesingle/1]).

% Some common RRLP packets, also should be used for testing I guess
%
% Ask for a GPS position:
% 

main(Args) ->
    S = lists:concat(Args),
    X = case just_zero_ones(S) of
        true -> parsebin(util:zeroones_to_bin(S));
        false -> parsebin(util:hex_to_bin(S))
    end,
    case X of
        {error, E} -> io:format("error: ~s~n", [E]);
        Otherwise -> io:format("~w~n", [Otherwise])
    end.

%% check for some common cases - given the whole RR APDU header, given
%% a wrong packet containing other RR messages.
%% Protocol Discriminators:
%%  0110 - RR (anything else we don't care about)
%% Message Types:
%%  00111000 - APDU
%%  00110100 - GPRS Suspension Request
parsebin(<< 0:4, 2#0110:4, 2#00111000:8, _APDI_ID:4, _APDU_Flags:4,
    _Length:8, Rest/binary >>) ->
    parsesingle(Rest);
parsebin(<< 0:4, 2#0110:4, 2#00110100:8, _Rest/binary >>) ->
    {error, "unexpected GPRS Suspension Request (RR Message)"};
parsebin(<< 0:4, 2#0110:4, Type:8, _Rest/binary >>) ->
    {error, io_lib:format("RR message, unexpected Type ~w", [Type])};
parsebin(<< 0:4, PD:4, _Rest/binary >>) ->
    {error, io_lib:format("not a RR message, PD ~w", [PD])};
% Any other case - just treat as a pure RRLP
% (4 byte "3byte header + 1 byte length) already removed)
parsebin(Other) -> parsesingle(Other).

parsesingle(Bin) ->
    io:format("~w bytes~n~s~n~s~n~w~n", [bit_size(Bin) div 8, "rrreccceoooooOmmaaaaaaappmmmu___", util:bin_to_zeroones(Bin), xrrlp:decode(Bin)]).

%% helpers

just_zero_ones([]) -> true;
just_zero_ones([$1|Rest]) -> just_zero_ones(Rest);
just_zero_ones([$0|Rest]) -> just_zero_ones(Rest);
just_zero_ones([_|_Rest]) -> false.

