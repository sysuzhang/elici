%%%-------------------------------------------------------------------
%%% @author rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2015 14:01
%%%-------------------------------------------------------------------
-module(elici_file).
-author("rongjie").


%% ====================================================================
%% API functions
%% ====================================================================
-export([open/2,path_open/3,close/1,format/3,request/1,compile_forms/2,write_file/2]).
-export([output/2, output/3, indent/2,
         output_pretty/2, output_pretty/3, output_pretty/4]).

open(File, Options) ->
	file:open(File,Options).

path_open(Path, File, Modes) ->
	file:path_open(Path, File, Modes).

close(FileRef) ->
	file:close(FileRef).

format(FileRef, FormatString, WriteFields) ->
	Msg = io_lib:format(FormatString, WriteFields),
	Bytes = unicode:characters_to_binary(Msg),
	file:write(FileRef, Bytes).
%io:format(FileRef, FormatString, WriteFields).

request(InFile) ->
	io:request(InFile,{get_until, unicode, prompt, elici_scanner,token,[1]}).

compile_forms(Forms, Options) ->
	compile:forms(Forms, [return] ++ Options).

write_file(File, Bytes) ->
	file:write_file(File,Bytes).


output(FileRef, Format, Data) ->
	Str = io_lib:format(Format, Data),
	output(FileRef, Str).
output(FileRef, Message) ->
	format(FileRef, "~ts", [Message]).

-spec output_pretty(FileRef :: port(), Message :: any()) -> ok.
output_pretty(FileRef, Message) ->
	output_pretty(FileRef, 0, Message).

-spec output_pretty(FileRef :: port(), Indent :: integer(), Message :: any()) -> ok.
output_pretty(FileRef, IndentNum, Message) ->
	ok = indent(FileRef, IndentNum),
	output(FileRef, Message).

-spec output_pretty(FileRef :: port(), Indent :: integer(), Format:: string(), Data :: list()) -> ok.
output_pretty(FileRef, IndentNum,Format, Data) ->
	ok = indent(FileRef, IndentNum),
	output(FileRef, Format, Data).

indent(FileRef, IndentNum) ->
	lists:foldl(fun(_, _Acc) -> format(FileRef, "~s", [" "])
	            end, ok, lists:seq(1, IndentNum)),
	ok.



%% ====================================================================
%% Internal functions
%% ====================================================================

