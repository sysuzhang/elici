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
script_execute(10, 0, []) ->
    T1=100*PlayerLv/Bless+1,
    case find_target() andalso scene_up() of 
        true -> 
            attack_target();
        _ -> 
                    none
    end    ;
%%{{body}}%%

script_execute(ScriptId, FunId, Args) ->
	exit({xscript_error, ScriptId, FunId, Args}).

%%{{local_function}}%%