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

//{{SCRIPT_FUN}}//


//{{LOCAL_FUNCTION}}//


}
