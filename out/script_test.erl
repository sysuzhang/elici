%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba>
%%% @doc
%%%    代码模板
%%%
%%% @end
%%% Created : 14. 八月 2015 16:37
%%%-------------------------------------------------------------------
-module(script_test).
-author("Rongjie").

-compile(export_all).
script_execute(ScriptId) ->
	script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
	script_execute(ScriptId, 0, Args).


%%测试
script_execute(10, 0, [SkillId,skilllv]) ->
    T1=find_target(),
    Fun0 = fun() -> 
        none
    end,
    wait(100, Fun0)    ;
%%{{body}}%%

script_execute(ScriptId, FunId, Args) ->
	exit({xscript_error, ScriptId, FunId, Args}).

%%{{local_function}}%%