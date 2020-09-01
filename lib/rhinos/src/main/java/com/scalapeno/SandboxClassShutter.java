package com.scalapeno;

import org.mozilla.javascript.ClassShutter;

public class SandboxClassShutter implements ClassShutter {

  public boolean visibleToScripts(String fullClassName) {
    return false;
  }

}
