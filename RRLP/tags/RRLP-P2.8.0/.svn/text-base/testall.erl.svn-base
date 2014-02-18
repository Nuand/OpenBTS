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


-module(testall).
-export([run/0]).

%%% dump out contents of all files in ../tests using asn1 generated code
run() ->
    make:all(),
    {ok,Files}=file:list_dir("../tests"),
	io:format("files=~p\n", [Files]),
	{ok,RE}=re:compile("\\.per"),
    lists:foreach(fun(X) -> runtest(X) end, [X||X<-Files,re:run(X,RE) /= nomatch]),
	io:format("*******************************\n").

runtest(Filename) ->
	io:format("******************************* running ~s\n", [Filename]),
    {ok,Contents}=file:read_file("../tests/" ++ Filename),
	io:format("Contents of ~s = ~w~n", [Filename, Contents]),
	case xrrlp:decode(Contents) of
		{error, Error} ->
			io:format("Fail: ~w\n", [Error]);
		Decode ->
			io:format("Success: ~w\n", [Decode]),
			XX = xrrlp:encodePDU(Decode),
			io:format("encode = ~w\n", [XX])
	end.
