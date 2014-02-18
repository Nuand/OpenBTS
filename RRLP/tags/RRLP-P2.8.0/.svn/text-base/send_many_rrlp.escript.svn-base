#!/usr/bin/env escript

main([IMSI]) ->
    main(IMSI, 0);
main([IMSI, Start]) ->
    main(IMSI, list_to_integer(Start));
main([]) ->
    io:format("usage: ./send_many_rrlp.escript IMSI~n").

main(IMSI, Start) ->
    [{io:format("~s~n", [integer_to_list(Accuracy)]),
      sendrrlpbody:main([IMSI, Accuracy + Start, 1 + Accuracy rem 6]), timer:sleep(20000)} ||
        Accuracy <- lists:seq(0,127) ].

% Sat Aug 22 02:47:57 PDT 2009
% Niv's place:
% Header (relative to APDU)
% 0000  0110  00111000 00000000 00010110
%      PD=RR  MTI=APDU  ID=0  LEN= 22

% RRLP Starts here
% 001                         0                           001
% Ref=1 Component: No extension  Measurement Request Response

% Measurement Request Response Starts here 0000100011111111111111111010101100001010110011111101101100100000011010111010100101101101010100100001110010100000000000000001000000000100000001001010011000000100100010000

% Parsed as (Lat is sign wrong, and Lon should be treated as 360 - X, i.e. -122
% {37.84985303878784,237.73487091064453,8}

% Sat Aug 22 03:11:04 PDT 2009
% {37.84987449645996,237.7348279953003,8}

% This is another honest to got MsrRequestRsp, says: LocationError, Not Enough Sattelites
% 00000110001110000000000000000011100000100000010000001000
%

% Could really use a "how many messages waiting" method before sending something.
