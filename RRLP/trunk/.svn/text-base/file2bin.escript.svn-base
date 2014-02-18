#!/usr/bin/env escript

main([Filename]) ->
    {ok,Data} = file:read_file(Filename),
	io:format("~s~n", [<< <<($0+X):8>> || <<X:1>> <= Data >>]).
