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

    //测试
    public ServerScriptBase Script_10(object SkillId,object skilllv)
    {
        T=find_target();
        var Fun0 = new TimerCallback((state) => {
            null; });
        wait(100, Fun0)        ;
    }
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
