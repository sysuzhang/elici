%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba>
%%% @doc
%%%    代码模板
%%%
%%% @end
%%% Created : 14. 八月 2015 16:37
%%%-------------------------------------------------------------------
-module(elici_template).
-author("Rongjie").

-compile(export_all).
script_execute(ScriptId) ->
	script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
	script_execute(ScriptId, 0, Args).

%%{{body}}%%

script_execute(ScriptId, FunId, Args) ->
	exit({xscript_error, ScriptId, FunId, Args}).

%%{{local_function}}%%