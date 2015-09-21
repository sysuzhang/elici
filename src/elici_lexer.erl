%%%-------------------------------------------------------------------
%%% @author rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc 词法分析
%%%
%%% @end
%%% Created : 29. 七月 2015 15:27
%%%-------------------------------------------------------------------
-module(elici_lexer).
-author("rongjie").
-include("elici_log.hrl").

%% API
-export([file/1, string/1]).

-spec file(Filename) -> {ok, Tokens, Lines} when
	Filename :: string() | pid(),
	Tokens :: list(tuple()),
	Lines :: integer().
%% 对文件进行词法分析
file(Filename) when is_list(Filename)->
	io:format("~nOpen File:~ts", [Filename]),
	case elici_file:open(Filename, [read]) of
		{ok, File} ->file(File);
		Err ->
			?EERROR("Open File Fail~p", [Err]),
			exit(open_file_fail)
	end;

file(File) when is_pid(File) ->
	{ok, Tokens, Line} =file(File, []),
	file:close(File),
	{ok, Tokens, Line}.
file(File, Acc) ->
	%% case io:request(File,{get_until,prompt, elici_scanner,token,[]}) of
	case elici_file:request(File) of
		{ok, Token, _Line} ->
			%?EINFO("Read:~p~n", [Token]),
			file(File, [Token|Acc]);
		{error, token} ->
			exit(scanning_error);
		{error, Err} ->
			?THROW({elici_lexer_fail, Err});
		{eof, Line} ->
			{ok, lists:reverse(Acc), Line}
	end.

-spec string(String) -> {ok, Tokens, Lines} when
	String :: string(),
	Tokens :: list(tuple()),
	Lines :: integer().
string(String) ->
	elici_scanner:string(String).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

lexer_test() ->
	%% File = "../example/hello.ec",
	?assertMatch({ok, _, _}, file("../example/hello.ec")),
	%%?assertEqual(ok, lexer("../example/array.ec")),
	ok.


-endif.


