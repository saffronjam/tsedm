%%%-------------------------------------------------------------------
%%% @author emil
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Sep 2022 15:02
%%%-------------------------------------------------------------------
-module(rudyfile).
-author("emil").

-define(WebRoot, "./webroot").

%% API
-export([get_file_by_uri/1]).

get_file_by_uri(RawUri) ->

  Uri = uri_preprocess(RawUri),

  Result = check_exists(Uri),
  case Result of
    {ok, FullPath} ->
      Content = read_file(FullPath),
      Size = filelib:file_size(FullPath),
      FileType = get_file_type(Uri),
      ContentType = get_content_type(FileType),

      {ok, Content, Size, ContentType};
    {error, Reason} -> % Could not find file
      {error, Reason}
  end.

uri_preprocess(Uri) ->
  %% If '/' is supplied, we treat it as '/index.html'
  EmptyUri = string:equal(Uri, "/"),
  if EmptyUri -> "/index.html"; true -> Uri end.

check_exists(URI) ->
  Path = lists:delete($/, URI), % URI starts with '/', so we need to remove that before searching filesystem
  filelib:find_file(Path, ?WebRoot).

read_file(Path) ->
  Result = file:read_file(Path),
  case Result of
    {ok, Content} ->
      binary_to_list(Content);
    {error, _} ->
      ""
  end.


get_file_type(Path) ->
  IsHtml = lists:suffix(".html", Path),
  IsJavascript = lists:suffix(".js", Path),
  IsCss = lists:suffix(".css", Path),
  if
    IsHtml -> html;
    IsJavascript -> javascript;
    IsCss -> css;
    true -> unknown
  end.

get_content_type(FileType) ->
  case FileType of
    html -> "text/html";
    javascript -> "text/javascript";
    css -> "text/css";
    unknown -> "text/plain"
  end.