%%%-------------------------------------------------------------------
%%% @author  Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 14. 八月 2015 10:49
%%%-------------------------------------------------------------------
-module(elici_util).
-author("Rongjie").

%% API
-export([reload/1, comment/1, rootname/1,string_to_term/1,
         re_replace/3]).
-export([format/3, concat/1, concat/2, concat_r/1, space/1]).
-export([function_exist/2]).
-export([new_line/0]).
-export([recursively_list_dir/1]).
reload(Modules) when is_list(Modules) ->
	lists:map(fun reload/1, Modules);
reload(Module) ->
	LoadRet =
	case is_changed(Module) of
		false ->
			unchanged;
		true ->
			code:purge(Module),
			case code:load_file(Module) of
				{module, Module} ->
					io:format("Reloading ~p ...", [Module]);
				{error, Reason} ->
					io:format(" Reload fail: ~p.~n", [Reason]),
					error
			end
	end,
	{Module, LoadRet}.

%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
	try
		module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
	catch _:_ ->
		false
	end.

%% Internal API

module_vsn({M, Beam, _Fn}) ->
	{ok, {M, Vsn}} = beam_lib:version(Beam),
	Vsn;
module_vsn(L) when is_list(L) ->
	{_, Attrs} = lists:keyfind(attributes, 1, L),
	{_, Vsn} = lists:keyfind(vsn, 1, Attrs),
	Vsn.


format(SpaceNum, Format, Data) ->
	Space = space(SpaceNum),
	Str = io_lib:format(Format, Data),
	Space ++ Str.

new_line()->
    "\n".

concat(SpaceNum, StringList) when SpaceNum =< 0 ->
	concat(StringList);
concat(SpaceNum, StringList) ->
	concat([space(SpaceNum)| StringList]).
concat(StringList) ->
	lists:foldr(fun(String, Acc) ->
		string:concat(String, Acc)
	end, "", StringList).


concat_r(StringList) ->
	lists:foldl(fun(String, Acc) ->
		string:concat(String, Acc)
	            end, "", StringList).

space(Number) ->
	lists:foldl(fun(_, Acc) -> string:concat(Acc, " ")
	            end, "", lists:seq(1, Number)).

re_replace(Subject, ReList, ReplacementList) ->
	{_, Result} = lists:foldl(fun(Re, {N, SubAcc}) ->
		%%io:format("~nSubAcc:~p", [SubAcc]),
		{N+1, re:replace(SubAcc, Re, lists:nth(N, ReplacementList), [{return, list}])}
	                          end, {1, Subject}, ReList),
	Result.

-spec comment(Filename :: string()) -> [Comment :: binary()].
%%获得文件名包含的注释
comment(Filename) ->
	try
		Length = max(string:rchr(Filename, $.), 1) - 1,
		SubFile = string:substr(Filename, 1, Length),
		string:sub_string(filename:extension(SubFile), 2)
	of
		Comment ->
			%%Comment
			CommentBin = unicode:characters_to_binary(Comment),
			[unicode:characters_to_binary(io_lib:format("~ts", [CommentBin]))]
	catch
		_:_ ->
			[<<"no comment">>]
	end.

%%第1个.号之前是rootname
rootname(Filename) ->
	case filename:rootname(Filename) of
		Filename ->
			filename:basename(Filename);
		Rootname ->
			rootname(Rootname)
	end.

%% https://gist.github.com/mrinalwadhwa/1059710
-spec recursively_list_dir(Dir) -> {ok, DirList, FileList} when
	Dir :: string(),
	DirList :: list(string()),
	FileList :: list(string()).
recursively_list_dir(Dir) ->
	recursively_list_dir([Dir], [], []). % default value of FilesOnly is false

recursively_list_dir([], DirAcc, FileAcc) ->
	{ok, DirAcc, FileAcc};
recursively_list_dir([Path|Paths], DirAcc, FileAcc) ->
	case filelib:is_file(Path) of
		true ->
			case filelib:is_dir(Path) of
				true ->
					{ok, Listing} = file:list_dir(Path),
					SubPaths = [filename:join(Path, Name) || Name <- Listing],
					recursively_list_dir(lists:append(SubPaths, Paths), [Path|DirAcc], FileAcc);
				false ->
					recursively_list_dir(Paths, DirAcc, [Path | FileAcc])
			end;
		false ->
			recursively_list_dir(Paths, DirAcc, FileAcc)
	end.

function_exist(_FunctionName, _Arg) ->
	true.

string_to_term(String) ->
	{ok, T, _} = erl_scan:string(String++"."),
	case erl_parse:parse_term(T) of
		{ok, Term} ->
			Term;
		{error, Error} ->
			Error
	end.