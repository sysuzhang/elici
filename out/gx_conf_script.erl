%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba>
%%% @doc
%%%    代码模板
%%%
%%% @end
%%% Created : 14. 八月 2015 16:37
%%%-------------------------------------------------------------------
-module(gx_conf_script).
-author("Rongjie").

-compile(export_all).
script_execute(ScriptId) ->
	script_execute(ScriptId, 0, []).
script_execute(ScriptId, Args) ->
	script_execute(ScriptId, 0, Args).


%%普通攻击
script_execute(301110, 0, [SkillID,TargetKey,Pos]) ->
    case gx_script_skill:can_apply_skill(SkillID,TargetKey) of 
        true -> 
            gx_script_effect:object_effect(gx_script_effect:get_effect_id(SkillID)),
            TargetPos1=gx_script_scene:get_current_pos(TargetKey),
            Scope1=gx_script_scene:make_box(Pos,200,150),
            Targetlist1=gx_script_scene_find:find_target(Scope1),
            Fun0 = fun() -> 
                gx_script_battle:attack_target(SkillID,Targetlist1)
            end,
            gx_script_common:wait(300,301110, Fun0);
        _ -> 
            gx_script_common:error(check_skill_fail)
    end    ;

%%普通攻击
script_execute(301120, 0, [SkillID,TargetKey,Pos]) ->
    case gx_script_skill:can_apply_skill(SkillID,TargetKey) of 
        true -> 
            gx_script_effect:object_effect(gx_script_effect:get_effect_id(SkillID)),
            TargetPos1=gx_script_scene:get_current_pos(TargetKey),
            Scope1=gx_script_scene:make_box(Pos,200,150),
            Targetlist1=gx_script_scene_find:find_target(Scope1),
            Fun0 = fun() -> 
                gx_script_battle:attack_target(SkillID,Targetlist1)
            end,
            gx_script_common:wait(300,301120, Fun0);
        _ -> 
            gx_script_common:error(check_skill_fail)
    end    ;
%%{{body}}%%

script_execute(ScriptId, FunId, Args) ->
	exit({xscript_error, ScriptId, FunId, Args}).

%%{{local_function}}%%