/* $Id: Continuation.java 208 2009-11-24 08:39:09Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2008 Michael G. Binz
 */
package de.michab.scream;

import java.util.logging.Level;
import java.util.logging.Logger;





/**
 * Represents a Scheme continuation.  Inside this class you are in
 * Dr. Caligari's cabinet. Beware!  Each line took weeks to write, but in
 * the end it works.
 *
 * Examples for the application of continuations can be found at:
 * http://www.scheme.com/tspl2d/further.html. To ease understanding smoke
 * plastic.
 *
 * @version $Rev: 208 $
 * @author Michael Binz
 */
public class Continuation extends Procedure
{
  /**
   * The logger for this class.
   */
  private final static Logger _log =
    Logger.getLogger( Continuation.class.getName() );

  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#getTypename()
   */
  public static final String TYPE_NAME = "continuation";

  /**
   * The embedded continuation.
   */
  private org.apache.commons.javaflow.Continuation _continuation = null;



  /**
   * A class that glues Javaflow into Scream. Note that only a single instance
   * of this class exists even in case of nested restarted continuations.
   */
  private static class Info
  {
    org.apache.commons.javaflow.Continuation __root;



    /**
     * Result handling.
     */
    private Object __result;



    /**
     *
     * @param result
     */
    void setResult( FirstClassObject result )
    {
      if ( _log.isLoggable( Level.FINE ) )
        _log.fine( "setResult: " + result + " " + this );

      __result = result;
    }



    /**
     *
     * @param error
     */
    void setResult( Error error )
    {
      __result = error;
    }



    /**
     *
     * @param runtimeError
     */
    void setResult( RuntimeX runtimeError )
    {
      __result = runtimeError;
    }



    /**
     *
     * @return
     * @throws RuntimeX
     */
    private FirstClassObject getResult()
        throws RuntimeX
    {
      if ( __result instanceof RuntimeX )
      {
        if ( _log.isLoggable( Level.FINE ) )
          _log.fine( "getResult: RuntimeX: " + __result );
        throw (RuntimeX)__result;
      }
      else if ( __result instanceof Error )
      {
        if ( _log.isLoggable( Level.FINE ) )
          _log.fine( "getResult: Error: " + __result );
        throw (Error)__result;
      }

      if ( _log.isLoggable( Level.FINE ) )
        _log.fine( "getResult: Fco = " + __result );

      return (FirstClassObject)__result;
    }
  };



  /**
   * A trivial glue class.
   */
  private static class Runner implements Runnable
  {
    /**
     * The root expression evaluated in this runner.
     */
    private FirstClassObject __expression;

    /**
     * The environment used to evaluate the root expression.
     */
    private Environment __env;



    /**
     * Create an instance.
     *
     * @param fco The fco to evaluate on this continuation.
     * @param env The environment to use for evaluation.
     */
    private Runner( FirstClassObject fco, Environment env )
    {
      __expression = fco;
      __env = env;
    }



    /**
     * Runs the expression passed in the constructor inside the Javaflow
     * continuation and places the result in the Info object expected to
     * be in the Javaflow context.
     */
    public void run()
    {
      try
      {
        FirstClassObject result =
          FirstClassObject.evaluate( __expression, __env );

        getInfo().setResult( result );
      }
      catch ( RuntimeX e )
      {
        getInfo().setResult( e );
      }
      catch ( Error e )
      {
        _log.log( Level.WARNING, "Unexpected Error", e );
        getInfo().setResult( e );
      }
    }
  };



  /**
   * An Error used for exit continuations. Transports a result as fast as
   * possible down the stack. Similar to scream.Unwind.
   */
  private class Escape extends Error
  {
    /**
     * A default serialization ID.
     */
    private static final long serialVersionUID = 1L;

    /**
     * The embedded object to be returned.
     */
    private final FirstClassObject _result;

    /**
     *
     * @param result
     */
    private Escape( FirstClassObject result )
    {
      _result = result;
    }
  }

  /**
   * A name used for the continuation procedure passed in the the procedure
   * specified in call-with-current-continuation.
   */
  private static final Symbol OPERATION_NAME = Symbol
      .createObject( "cc/escape" );



  /**
   * Create an instance.
   */
  private Continuation()
  {
    super( OPERATION_NAME );
  }



  @Override
  public Object convertToJava()
  {
    return _continuation;
  }



  @Override
  public String toString()
  {
    StringBuilder sb = new StringBuilder( "<" );
    sb.append( Continuation.TYPE_NAME );
    sb.append( ':' );
    sb.append( _continuation.hashCode() );
    sb.append( ">" );

    return sb.toString();
  }



  /**
   * Evaluates the passed arguments in a continuation and returns the result.
   *
   * @param fco
   *          The expression to evaluate.
   * @param env
   *          The Environment for the evaluation.
   * @return The result of the evaluation.
   * @throws If
   *           the evaluation failed.
   */
  public static FirstClassObject begin( FirstClassObject fco, Environment env )
      throws RuntimeX
  {
    Info info = new Info();

    info.__root = org.apache.commons.javaflow.Continuation
        .startSuspendedWith( new Runner( fco, env ) );

    // A return from continueWith below with a non-null result means that
    // a call/cc call has been left.  If the result is null this means that
    // the computation has been finished normally.
    do
    {
      info.__root = org.apache.commons.javaflow.Continuation.continueWith(
          info.__root,
          info );

    } while ( info.__root != null );

    return info.getResult();
  }



  /**
   * Activates the passed procedure and makes sure that the Unwind the tail
   * calls are based on are handled here. This is needed to prevent error
   * leakage to lower stack frames, disturbing our continuation handling.
   *
   * @param env
   *          The activation environment.
   * @param op
   *          The operation to activate.
   * @param c
   *          The parameter to pass to the operation.
   * @return The operation result.
   * @throws RuntimeX
   *           In case an evaluation error occurred.
   */
  private static FirstClassObject callccUnwindBarrier( Environment env,
      Procedure op, Continuation c ) throws RuntimeX
  {
    try
    {
      return op.activate(
          env,
          new FirstClassObject[]{ c } );
    }
    catch ( Unwind e )
    {
      return e.result();
    }
  }



  /**
   * Called if call/cc is entered.
   *
   * @return The call/cc result.
   */
  private static FirstClassObject callcc( Environment env, Procedure op )
      throws RuntimeX
  {
    Continuation c = new Continuation();

    try
    {
      getInfo().setResult( callccUnwindBarrier( env, op, c ) );
    }
    catch ( RuntimeX ce )
    {
      getInfo().setResult( ce );
    }
    catch ( Escape e )
    {
      getInfo().setResult( e._result );
    }
    catch ( Error e )
    {
      _log.log( Level.WARNING, "call/cc error", e );
      getInfo().setResult( e );
    }
    finally
    {
      // The suspend triggers dropping the stack into glue._root.
      org.apache.commons.javaflow.Continuation.suspend();
      // If the continuation is null, this means that we are leaving the
      // call/cc stack frame the first time. Since we work inside
      // a continuation, it is possible, that we leave more than once.
      // In this case, we do not want to reset our continuation.
      if ( c._continuation == null )
        c._continuation = getInfo().__root;
    }

    return getInfo().getResult();
  }



  private static Info getInfo()
  {
    return (Info)org.apache.commons.javaflow.Continuation.getContext();
  }



  /**
   * Called if the continuation is activated.
   */
  @Override
  protected final FirstClassObject apply( FirstClassObject[] argumentList )
      throws RuntimeX
  {
    checkArgumentCount( 1, argumentList );

    FirstClassObject result = argumentList[0];

    // If the continuation is null, this is an exit continuation. Simple.
    if ( _continuation == null )
      throw new Escape( result );

    _log.fine( "fcoApply: continueWith" );

    // This is a continuation restart.
    getInfo().setResult( result );
    org.apache.commons.javaflow.Continuation.continueWith(
        _continuation, getInfo() );

    return getInfo().getResult();
  }



  /**
   * (call-with-current-continuation <expression>)
   */
  static private Procedure callWithCurrentContinuationProcedure =
    new Procedure( "call-with-current-continuation" )
  {
    private Class<?>[] formalArglist = new Class<?>[]
    { Procedure.class };



    public FirstClassObject apply( Environment parent, FirstClassObject[] args )
        throws RuntimeX
    {
      checkArguments( formalArglist, args );

      return callcc( parent, (Procedure) args[0] );
    }
  };



  /**
   * Environment operations setup.
   *
   * @param tle
   *          A reference to the system private top level environment.
   * @return A reference to the environment including the additional entries
   *         defined by this class.
   */
  public static Environment extendTopLevelEnvironment( Environment tle )
  {
    tle.setPrimitive( callWithCurrentContinuationProcedure );
    tle.set( Symbol.createObject( "call/cc" ),
        callWithCurrentContinuationProcedure );

    return tle;
  }
}
