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


-module(util).
-export([zeroones_to_bin/1,bin_to_zeroones/1,hex_to_bin/1,bin_to_uhex_string/1]).

% technically this should be (X-$0), but since $0 == 48, the
% zeroth bit is 0 for $0, and 1 for $1, so it works! :)
zeroones_to_bin(A) -> << <<X:1>> || X <- A >>.

% The slower version? At least the longer-to-write version
%zeroones_to_bin(A) -> zeroones_to_bin(A, <<>>).
%zeroones_to_bin([], Ans) -> Ans;
%zeroones_to_bin([$0|Rest], Ans) -> zeroones_to_bin(Rest, <<Ans/bits, 0:1>>);
%zeroones_to_bin([$1|Rest], Ans) -> zeroones_to_bin(Rest, <<Ans/bits, 1:1>>).

% just because it was fun:
bin_to_zeroones(Bin) -> << <<($0+X)>> || <<X:1>> <= Bin >>.

hex_to_bin(Bin) -> << <<(hex_digit_to_int(X)):4>> || X <- Bin >>.

hex_digit_to_int(X) when X >= $0, X =< $9 -> X - $0;
hex_digit_to_int(X) when X >= $A, X =< $F -> X - $A + 10;
hex_digit_to_int(X) when X >= $a, X =< $f -> X - $a + 10.

% uhex == upper case hex
bin_to_uhex_string(Bin) -> bitstring_to_list(
    << << (int_to_uhex_digit(X)):8 >> || << X:4 >> <= Bin >> ).

int_to_uhex_digit(X) when X >= 0, X =< 9 -> $0 + X;
int_to_uhex_digit(X) when X >= 10, X =< 15 -> $A + X - 10.

