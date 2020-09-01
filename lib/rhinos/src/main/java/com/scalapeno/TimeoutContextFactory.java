package com.scalapeno;

import org.mozilla.javascript.Callable;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.Scriptable;

/**
 * Creates a Rhino #{@link Context} which will timeout after a specified number of milliseconds.
 */
public class TimeoutContextFactory extends ContextFactory {

  /** Default is 10 seconds **/
  private static final long DEFAULT_SECONDS_TIMEOUT = 3;
  private static final long DEFAULT_MILLISECONDS_TIMEOUT = DEFAULT_SECONDS_TIMEOUT * 1000;

  private final long millisecondTimeout;

  public TimeoutContextFactory() {
    this(DEFAULT_MILLISECONDS_TIMEOUT);
  }

  /**
   * Create a new TimeoutContextFactory with specified number of milliseonds for timeout
   * @param millisecondsTimeout number of milliseconds before timeout
   */
  public TimeoutContextFactory(long millisecondsTimeout) {
    this.millisecondTimeout = millisecondsTimeout;
  }

  @Override
  protected void observeInstructionCount(Context context, int instructionCount) {
    TimeoutContext timeoutContext = (TimeoutContext)context;
    long currentTime = System.currentTimeMillis();
    if (currentTime - timeoutContext.startTime > millisecondTimeout) {
      throw new ThreadDeath();
    }
  }

  @Override
  protected Context makeContext() {
    TimeoutContext timeoutContext = new TimeoutContext(this);
    timeoutContext.setInstructionObserverThreshold(10000);
    return timeoutContext;
  }

  protected Object doTopCall(Callable callable, Context context, Scriptable scope, Scriptable thisObj, Object[] args)
  {
    TimeoutContext timeoutContext = (TimeoutContext)context;
    timeoutContext.startTime = System.currentTimeMillis();
    return super.doTopCall(callable, context, scope, thisObj, args);
  }

  /**
   * Context subclass which is aware of when it was started.
   */
  private static class TimeoutContext extends Context {

    long startTime;

    public TimeoutContext(ContextFactory contextFactory) {
      super(contextFactory);
    }

  }

}
