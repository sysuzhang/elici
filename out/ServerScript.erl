using UnityEngine;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Reflection;
using Network;
class ServerScript
{
    private static ServerScript instance;

    public static ServerScript Instance
    {
        get
        {
            if (instance == null)
            {
                instance = new ServerScript();
            }
            return instance;
        }
    }

    public object execute(int scriptId) 
    {
        return this.execute(scriptId, null);
    }
    public object execute(int scriptId, object[] paramaters)
    {
        string funName = "Script_" + scriptId;
        return this.invoke(funName, paramaters);
    }

    public object invoke(string functionName, object[] paramaters)
    {
        Type type = typeof(ServerScript);
        MethodInfo methodInfo = type.GetMethod(functionName);
        return methodInfo.Invoke(this, paramaters);
    }

    //普通攻击    public ServerScriptBase Script_301110(object SkillInfo,object TargetKey,object Pos)
    {
        SkillID=gx_script_skill:get_skill_id(SkillInfo);
        if(gx_script_skill:can_apply_skill(SkillID,TargetKey)){
                 gx_script_effect:notify_obj_effect(gx_script_effect:get_effect_id(SkillID));
                TargetPos=gx_script_scene:get_current_pos(TargetKey);
                Scope=gx_script_scene:make_box(Pos,200,150);
                Targetlist=gx_script_scene_find:find_target(Scope);
                var Fun0 = new TimerCallback((state) => {
                    gx_script_battle:attack_target(SkillID,Targetlist); });
                wait(300, Fun0); } else {
         gx_script_common:error(check_skill_fail); }    }
//{{SCRIPT_FUN}}//

    //各个脚本代码
    public ServerScriptBase Script_301110(int skillId, long targetKey, pb_object_pos pos)
    {
        //SkillID = get_skill_id(SkillInfo); 这一行的翻译免了,需要重写脚本
        if (ServerScriptSkill.Instance.can_apply_skill(skillId, targetKey))
        {
            ServerScriptEffect.Instance.notify_obj_effect(ServerScriptSkill.Instance.get_effect_id(skillId));
            var Fun0 = new TimerCallback((state) =>
            {
                ServerScriptBattle.Instance.attack_target(skillId, targetKey);
            });
            ServerScriptCommon.Instance.wait(300, Fun0);
        }
        else
        {
            ServerScriptCommon.Instance.error(1, 0x01ff, "使用技能出错");
        }

        return new ServerScriptBase();
    }






}
