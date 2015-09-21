%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2015 10:49
%%%-------------------------------------------------------------------
-module(elici_compiler).
-author("Rongjie").
-include("elici.hrl").

%% API
-export([debug/1, debug/2]).
-export([forms/1, dir/1, dir/2, file/1, file/2]).

%% debug method
debug(Info) ->
	erlang:display(Info).

debug(Format, Data) ->
	Info = io_lib:format(Format, Data),
	erlang:display(lists:flatten(Info)).

-spec forms(File :: string() | pid()) -> {ok, SyntaxForms :: list()}.
%%得到的结果是forms
forms(File) ->
	case elici_lexer:file(File) of
		{ok, Tokens, _Lines} ->
			%%debug(Tokens),
			case elici_syntax:parser(Tokens) of
				{ok, SyntaxForm} ->
					{ok, SyntaxForm};
				Err2 ->
					erlang:display(Err2),
					?EERROR("Syntax Error:~p", [Err2]),
					exit(elici_parser_fail)
			end;
		Err1 ->
			?EERROR(Err1),
			exit(elici_scan_fail)
	end.

%% 处理指定目录的脚本
dir(Dirname) ->
	dir(Dirname, []).

dir(Dirname, Options) ->
	file(Dirname, Options).

file(FileNames) ->
	file(FileNames, []).

%% 解析参数,设置默认参数
parse_options(Options) ->
	DefalutOptions = lists:foldl(fun({DefalutKey, _} = Default, Acc) ->
		case proplists:is_defined(DefalutKey, Options) of
			true -> Acc;
			false -> [Default | Acc]
		end
	end, [{work_module,elici_translate_erlang},
	      {out_dir, "./"},
	      {out_file, "elici_default.out"}], Options),
	Options ++ DefalutOptions.


file(FileNames, Options) ->
	NewOptions = parse_options(Options),
	WorkModule = proplists:get_value(work_module, NewOptions, elici_translate_erlang),
	OutDir = proplists:get_value(out_dir, NewOptions),
	OutFilename = proplists:get_value(module, Options,  "elici.out"),
	Extern = proplists:get_value(ext, Options, ".erl"),
	ModuleName = atom_to_list(OutFilename),


	TemplateFileName = proplists:get_value(template, Options),
	{ok,Template} =file:read_file(TemplateFileName),
	NewTemplate = re:replace(Template, "elici_template", ModuleName),

	Elici = #elici{template = NewTemplate, out_dir = OutDir, work_module = WorkModule},
	NewElici =
	lists:foldl(fun(Filename, Acc) ->
		{ok, SyntaxForm} = forms(Filename),
		ScriptID = elici_util:rootname(Filename),
		case catch elici_translate:forms(WorkModule, SyntaxForm, Acc#elici{filename = Filename, scriptid = ScriptID, bindings = #{}, indent = 0}) of
			#elici{content = Content} = NewAcc ->
				NewAcc#elici{template = Content, content = []};
			?ERROR(Msg) ->
				io:put_chars(Filename),
				exit({translate_file_fail, Msg});
			_Other ->
				io:put_chars(Filename),
				exit({unknow_error, _Other})
		end
	            end, Elici, FileNames),
	%%Write To file:
	OutFile = filename:join(OutDir, ModuleName ++ Extern),
	io:format("~nOutFile:~p", [OutFile]),
	{ok, File} = elici_file:open(OutFile, [write]),
	file:write(File, NewElici#elici.template),
	elici_file:close(File),

	%%自动编译
	case proplists:get_value(compile_dir, Options) of
		undefined -> ok;
		CompileDir ->
			io:format("~nCompile File:~p.......~n", [OutFile]),
			CompileOptions = [
				{outdir, CompileDir},
				nowarn_unused_vars
			],
			c:c(OutFile, CompileOptions)
	end,
	ok.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


file_test() ->
	?assertEqual(ok, file("../example/csharp.ec")).



-endif.