package com.scalapeno;

import org.mozilla.javascript.Context;

public class SandboxContextFactory extends TimeoutContextFactory {

  @Override
  protected Context makeContext() {
    Context context = super.makeContext();
    context.setWrapFactory(new SandboxWrapFactory());
    context.setClassShutter(new SandboxClassShutter());
    return context;
  }

}
