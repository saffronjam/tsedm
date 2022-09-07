%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2022 12:47
%%%-------------------------------------------------------------------
-module(http).
-author("emil").

%% API
-export([parse_request/1, ok/1, ok_file/3, not_found/0]).


parse_request(R0) ->
  {Request, R1} = request_line(R0),
  {Headers, R2} = headers(R1),
  {Body, _} = message_body(R2),
  {Request, Headers, Body}.

request_line([$G, $E, $T, 32 | R0]) ->
  {URI, R1} = request_uri(R0),
  {Ver, R2} = http_version(R1),
  [13, 10 | R3] = R2, % 13 = Carriage Return, 10 = Line feed -> Which means we are done with the first part; request_line
  {{get, URI, Ver}, R3}.

request_uri([32 | R0]) -> % Base case is when we have reached the space character
  {[], R0};
request_uri([C | R0]) -> % We recursively iterate the request URI, which comes in form of a string, one character at a time
  {Rest, R1} = request_uri(R0),
  {[C | Rest], R1}. % Append current character to list of character (the final request URI string)

http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) -> % http version 1.1
  {v10, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) -> % http version 1.0
  {v11, R0}.

headers([13, 10 | R0]) -> % Continue parsing the group of headers until we reach CRLF, which marks the end of all the headers
  {[], R0};
headers(R0) ->
  {Header, R1} = header(R0),
  {Rest, R2} = headers(R1),
  {[Header | Rest], R2}.

header([13, 10 | R0]) -> % Continue with this specific header until we reach CRLF, which marks the end of a single header
  {[], R0};
header([C | R0]) ->
  {Rest, R1} = header(R0), % Similar to request URI, each header will recursively looked through and grouped. Stored as strings
  {[C | Rest], R1}.

message_body(R) -> % Treat the incoming part of the request as a body, this should be the last part of the request
  {R, []}.

ok(Body) ->
  "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

ok_file(ContentType, FileContent, FileSize) ->
  lists:concat(["HTTP/1.1 200 OK\r\nContent-Type: ", ContentType, "\r\nContent-Length: ", integer_to_list(FileSize), "\r\n\r\n", FileContent]).

not_found() ->
  "HTTP/1.1 404 Not Found\r\n" ++ "\r\n".

get(URI) ->
  "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n". % On CRLF to end status line, and one to the (empty) header section