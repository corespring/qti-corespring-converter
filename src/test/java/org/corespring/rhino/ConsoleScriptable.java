package org.corespring.rhino;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;
import org.mozilla.javascript.annotations.JSFunction;

public class ConsoleScriptable extends ScriptableObject {

    public ConsoleScriptable(){}

    @Override
    public String getClassName() {
        return "ConsoleScriptable";
    }

    @JSFunction
    public static void log(Context cx, Scriptable thisObj,
                                 Object[] args, Function funObj)
    {
        StringBuilder buf = new StringBuilder();
        for (int i=0; i < args.length; i++) {
            buf.append(Context.toString(args[i]));
            if (i+1 != args.length)
                buf.append(" ");
        }
        //System.out.println("[JS]> " + buf.toString());
    }
}
