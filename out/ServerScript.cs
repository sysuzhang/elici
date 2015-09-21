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

    //普通攻击
    public ServerScriptBase Script_301110(object SkillID,object TargetKey,object Pos)
    {
        if(ServerScriptSkill.Instance.can_apply_skill(SkillID,TargetKey)){
                 ServerScriptEffect.Instance.notify_obj_effect(ServerScriptEffect.Instance.get_effect_id(SkillID));
                var TargetPos=ServerScriptScene.Instance.get_current_pos(TargetKey);
                var Scope=ServerScriptScene.Instance.make_box(Pos,200,150);
                var Targetlist=ServerScriptSceneFind.Instance.find_target(Scope);
                var Fun0 = new TimerCallback((state) => {
                    ServerScriptBattle.Instance.attack_target(SkillID,Targetlist); });
                ServerScriptCommon.Instance.wait(300, Fun0); } else {
         ServerScriptCommon.Instance.error(check_skill_fail); }    }

    //普通攻击
    public ServerScriptBase Script_301120(object SkillID,object TargetKey,object Pos)
    {
        if(ServerScriptSkill.Instance.can_apply_skill(SkillID,TargetKey)){
                 ServerScriptEffect.Instance.notify_obj_effect(ServerScriptEffect.Instance.get_effect_id(SkillID));
                var TargetPos=ServerScriptScene.Instance.get_current_pos(TargetKey);
                var Scope=ServerScriptScene.Instance.make_box(Pos,200,150);
                var Targetlist=ServerScriptSceneFind.Instance.find_target(Scope);
                var Fun0 = new TimerCallback((state) => {
                    ServerScriptBattle.Instance.attack_target(SkillID,Targetlist); });
                ServerScriptCommon.Instance.wait(300, Fun0); } else {
         ServerScriptCommon.Instance.error(check_skill_fail); }    }

//{{SCRIPT_FUN}}//


//{{LOCAL_FUNCTION}}//


}
