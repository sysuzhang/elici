%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc 对外接口
%%%
%%% @end
%%% Created : 28. 七月 2015 20:49
%%%-------------------------------------------------------------------
-module(elici).
-author("Rongjie").
-include("elici.hrl").
-define(SERVER, elici_compile).

-export([start/0,
         l/0, test/0, test2/0]).
%% API
-export([file/2, string/2]).
-export([gen_erlang/0, gen_erlang/1,
         gen_csharp/0, gen_csharp/1]).
-export([get_module_csharp_map/1]).


start() ->
	%% observer:start(),
	case elici_compiler:start_link("rongjie") of
		{ok, CompilerPid} ->
			link(CompilerPid);
		Err ->
			erlang:display(Err)
	end.

file(Filename, Options) ->
	{ok, Bin} = file:read_file(Filename),
	string(binary_to_list(Bin), Options).

string(String, []) ->
	case elici_scanner:string(String) of
		{ok, _Tokens} -> ok;
		_ -> ok
	end,
	ok.


%% 生成Erlang代码
gen_erlang() ->
	l(),
	{ok, Options} = file:consult("./config/elici.config"),
	gen_erlang(Options).
gen_erlang(Options) ->
	%%io:setopts([{encoding, unicode}]),
	%%io:setopts([{encoding, latin1}]),
	l(),
	%%io:format("~nOptions:~p~n", [Options]),
	SrcDir = proplists:get_value(src_dir, Options, "./"),
	ScriptFiles = get_script_src(SrcDir),

	ErlangAPIDir =  proplists:get_value(erlang_api_dir, Options, "./"),
	ok = load_script_erlang_api(ErlangAPIDir),

	elici_compiler:file(ScriptFiles, [{work_module, proplists:get_value(erlang_work_module, Options, elici_translate_erlang)},
	                                  {template, proplists:get_value(erlang_template, Options, "./template/elici_template.erl")},
	                                  {module, proplists:get_value(erlang_mod_name, Options, gx_conf_script)},
	                                  {out_dir, proplists:get_value(dst_dir, Options, "./")},
	                                  {compile_dir, proplists:get_value(ebin_dir, Options, "./")},
	                                  {ext, ".erl"}] ++ Options),
	ok.

%% 生成Csharp代码
gen_csharp() ->
	{ok, Options} = file:consult("./config/elici.config"),
	gen_csharp(Options).

gen_csharp(Options) ->
	l(),
	SrcDir = proplists:get_value(src_dir, Options, "./"),
	ScriptFiles = get_script_src(SrcDir),
	CSharpAPI =  proplists:get_value(csharp_api, Options, "./"),
	ok = load_script_csharp_api(CSharpAPI),

	elici_compiler:file(ScriptFiles, [{work_module, proplists:get_value(csharp_work_module, Options, elici_translate_csharp)},
	                                  {template, proplists:get_value(csharp_template, Options, "./template/elici_template.cs")},
	                                  {module, proplists:get_value(csharp_class_name, Options, 'ServerScript')},
	                                  {out_dir, proplists:get_value(csharp_dir, Options, "./")},
	                                  {ext, ".cs"}] ++ Options),
	ok.



get_script_src(SrcDir) ->
	{ok, DirnameAll, _} = elici_util:recursively_list_dir(SrcDir),
	ScriptFiles =
	lists:foldl(fun(Dirname, Acc) ->
		Files1 = [filename:join(Dirname , X)|| X <- filelib:wildcard("*.script", Dirname)],
		lists:append(Files1, Acc)
	            end, [],DirnameAll),
	ScriptFiles.

load_script_erlang_api(ErlangAPIDir) ->
	true = code:add_path(ErlangAPIDir),

	io:format("~nErlangAPIDir:~p", [ErlangAPIDir]),
	BeamList = [filename:join(ErlangAPIDir , BeamFile) || BeamFile <-filelib:wildcard("gx_script_*.beam", ErlangAPIDir)],
	io:format("BeamList:~p", [BeamList]),

	%% 加载API
	lists:map(fun(Beam) ->
		StrMod = filename:basename(Beam, ".beam"),
		Mod = list_to_atom(StrMod),
		code:purge(Mod),
		Beam2 = filename:rootname(Beam),
		case code:load_abs(Beam2) of
			{error, What} -> exit(What);
			_ -> ok
		end,
		Mod
	          end, BeamList),
	ok.

load_script_csharp_api(CSharpAPI) ->
	%%io:format("~nCSharp:~p", [CSharpAPI]),
	erlang:erase(csharp_api),
	CSharpAPIDefine = case erlang:get(csharp_api) of
						  undefined -> orddict:new();
						  Val -> Val
	                  end,
	NewVal =
	lists:foldl(fun({Key, Map, Define}, Acc) ->	orddict:store(Key, {Map, Define}, Acc)
	            end, CSharpAPIDefine, CSharpAPI),
	erlang:put(csharp_api, NewVal),
	ok.

get_module_csharp_map(Mod) ->
	case erlang:get(csharp_api) of
		undefined -> throw(?ERROR({no_load_csharp_api, Mod}));
		Val ->
			case orddict:find(Mod, Val) of
				error -> throw(?ERROR({no_define_csharp_mapping, Mod}));
				{ok, {Map, _}} -> {ok, Map}
			end
	end.


l() ->
	elici_util:reload([elici, elici_util, elici_binds, elici_compiler, elici_file, elici_lexer, elici_parser, elici_refactor,
	                  elici_scanner, elici_syntax, elici_translate, elici_translate_erlang, elici_translate_csharp]),
	ok.

test() ->
	l(),
	Options = [],
	ScriptFiles = ["./example/10.csharp.测试.ec"],
	elici_compiler:file(ScriptFiles, [{work_module, proplists:get_value(erlang_work_module, Options, elici_translate_erlang)},
	                                  {template, proplists:get_value(erlang_template, Options, "./template/elici_template.erl")},
	                                  {module, proplists:get_value(erlang_mod_name, Options, script_test)},
	                                  {out_dir, proplists:get_value(dst_dir, Options, "./")},
	                                  {compile_dir, proplists:get_value(ebin_dir, Options, "./")},
	                                  {ext, ".erl"}] ++ Options).



test2() ->
	l(),
	Options = [],
	ScriptFiles = ["./example/10.csharp.测试.ec"],
	elici_compiler:file(ScriptFiles, [{work_module, proplists:get_value(csharp_work_module, Options, elici_translate_csharp)},
	                                  {template, proplists:get_value(csharp_template, Options, "./template/elici_template.cs")},
	                                  {module, proplists:get_value(csharp_class_name, Options, 'ServerScript')},
	                                  {out_dir, proplists:get_value(csharp_dir, Options, "./")},
	                                  {ext, ".cs"}] ++ Options).