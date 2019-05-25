/* $Id: FirstClassObject.java 180 2009-06-06 15:50:36Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;



/**
 * <p>The base class for all Scheme first-class objects.  A first class object is
 * an object that can be bound to a symbol.</p>
 * <p>The static versions of the methods on this object provide additional
 * NIL handling over the non-static ones.</p>
 * <p>Implementations of FirstClassObjects represent types in Scheme.  Each
 * implementation has to offer a string attribute named TYPE_NAME as a static
 * final member for full integration with type conversion messages.</p>
 */
public abstract class FirstClassObject
{
  /**
   *
   */
  private boolean _isConstant;



  /**
   * Helper used for implementation of tail recursiveness.  The exception is
   * used for control flow purposes.  The reason for its <code>Error</code>
   * base class is to keep the inner workings of that invisible to the user.
   * <br>
   * An instance is only thrown in the <code>static evaluateTailContext()</code>
   * method and always catched in the <code>static evaluate()</code>
   * method.<br>
   * The purpose of this class is to be able to explicitly unwind one
   * stackframe of the Java VM stack to be able to execute recursive Scheme
   * procedures in constant stack space.
   */
  static class Unwind
    extends Error
  {
    /**
     * The embedded object to be evaluated.
     */
    private final FirstClassObject _fco;



    /**
     * The embedded environment to be used for evaluation.
     */
    private final Environment _environment;



    /**
     * Creates an instance.
     *
     * @param fco The object to be evaluated.
     * @param e The environment to use for evaluation.
     */
    Unwind( FirstClassObject fco, Environment e )
    {
      _fco = fco;
      _environment = e;
    }


    FirstClassObject result()
        throws RuntimeX
    {
        return evaluate( _fco, _environment );
    }
  }



  /**
   * Evaluates the passed scheme object and returns the result.  Handles a NIL
   * object to evaluate.
   *
   * @param fco The FirstClassObject to evaluate.
   * @param env The environment for the evaluation.
   * @return The result of the evaluation.
   * @throws RuntimeX In case the evaluation failed.
   */
  public static FirstClassObject evaluate( FirstClassObject fco, Environment env )
    throws RuntimeX
  {
    while ( true )
    {
      try
      {
        if ( fco != Cons.NIL )
          return fco.evaluate( env );
        else
          return Cons.NIL;
      }
      catch ( Unwind e )
      {
        // After we received a stack unwind notification (which is technically
        // an Error), our local parameters get updated with the received
        // information and evaluation is done in the next loop cycle.
        fco = e._fco;
        env = e._environment;
      }
    }
  }



  /**
   * Does the evaluation of a trailing context.  That means that the current
   * stackframe will be removed before the evaluation starts.
   *
   * @param fco The FirstClassObject to evaluate.
   * @param env The environment for the evaluation.
   * @return The result of the evaluation.
   * @throws RuntimeX In case the evaluation failed.
   * @see #evaluate( FirstClassObject fco, Environment e )
   */
  public static FirstClassObject evaluateTrailingContext(
      FirstClassObject fco,
      Environment env )
    throws RuntimeX
  {
    throw new Unwind( fco, env );
  }



  /**
   * Evaluate this scheme object and return the result.  This default version
   * just evaluates to itself.  This must only be called if it is ensured that
   * the target object is not null.  If in doubt call FCO.evaluate( FCO, FCO ).
   *
   * @param e The environment used to evaluate the object.
   * @return The result of the evaluation.
   * @throws RuntimeX In case the evaluation failed.
   */
  protected FirstClassObject evaluate( Environment e )
    throws RuntimeX
  {
    // The following code allows to interrupt an ongoing scheme computation.
    // Note that the interrupted() static method call resets the interrupted
    // flag of the current thread, so the thread will be able to do additional
    // work after handling the error message.
    if ( Thread.interrupted() )
      throw new RuntimeX( "INTERRUPTED" );

    return this;
  }



  /**
   * The implementation of the Scheme <code>eqv?</code> procedure.  This static
   * method also handles NIL references.
   *
   * @param left The left operand for the comparison.
   * @param right The right operand of the comparison.
   * @return The result of the comparison.
   */
  public static boolean eqv( FirstClassObject left, FirstClassObject right )
  {
    // If one of the references is NIL...
    if ( Cons.NIL == left || Cons.NIL == right )
      // ...do a reference compare.  This is only successful if both object
      // references are NIL.
      return left == right;
    else
      return left.eqv( right );
  }



  /**
   * The implementation of the Scheme <code>eqv?</code> procedure.  The default
   * implementation delegates the operation to the <code>eq()</code> method.
   *
   * @param other The object to compare with.
   * @return The result of the comparison.
   */
  protected boolean eqv( FirstClassObject other )
  {
    return eq( other );
  }



  /**
   * The implementation of the scheme <code>eq?</code> procedure.  Handles NIL
   * references.
   *
   * @param left The left operand for the comparison.
   * @param right The right operand of the comparison.
   * @return The result of the comparison.
   */
  public static boolean eq( FirstClassObject left, FirstClassObject right )
  {
    // If one of the references is NIL...
    if ( Cons.NIL == left || Cons.NIL == right )
      // ...do a reference compare.  This is only successful if both object
      // references are NIL.
      return left == right;
    else
      return left.eq( right );
  }



  /**
   * The implementation of the scheme eq? procedure.  The default compares the
   * references.
   *
   * @param other The object to compare with.
   * @return The result of the comparison.
   */
  protected boolean eq( FirstClassObject other )
  {
    return this == other;
  }



  /**
   * The implementation of the scheme equal? procedure.  Handles a NIL left
   * object.
   *
   * @param left The left operand for the comparison.
   * @param right The right operand of the comparison.
   * @return The result of the comparison.
   */
  public static boolean equal( FirstClassObject left, FirstClassObject right )
  {
    // If one of the references is NIL...
    if ( Cons.NIL == left || Cons.NIL == right )
      // ...do a reference compare.  This is only successful if both object
      // references are NIL.
      return left == right;
    else
      return left.equal( right );
  }



  /**
   * The implementation of the scheme equal? procedure.  The default is to
   * delegate the comparison to the eqv() method.
   *
   * @param other The object to compare with.
   * @return The result of the comparison.
   */
  protected boolean equal( FirstClassObject other )
  {
    return eqv( other );
  }



  /**
   * Create a string from the passed object suitable for the read() operation.
   * To be able to also convert NILs to a string representation this is
   * implemented as a static method.
   *
   * @param object The object to be transformed to its string representation.
   * @return The string representation of the object.
   * @see java.lang.Object#toString
   */
  static public String stringize( FirstClassObject object )
  {
    if ( Cons.NIL == object )
      return "()";
    else
      return object.toString();
  }



  /**
   * Create a string from this object suitable for the read() operation.
   *
   * @return The object's string representation.
   */
  public abstract String toString();



  /**
   * Clone this scheme object.  The default implementation just returns itself.
   * This <code>static</code> version of this method additionally handles NIL
   * references.
   *
   * @param fco The first class object to be cloned.
   * @return A clone for the passed object.  Note that it depends on the type
   *         of the object whether this is a real clone or identical to the
   *         object passed in.
   */
  public static Object clone( FirstClassObject fco )
  {
    if ( fco == Cons.NIL )
      return Cons.NIL;
    else
      return fco.clone();
  }



  /**
   * Clone this scheme object.  The default implementation just returns itself.
   * Note that it depends on the type of the object whether the returned object
   * is a real clone or the identity of the object.
   *
   * @return A clone for the object.
   */
  public Object clone()
  {
   // TODO: here we have a problem.  We want to be able to throw an RuntimeX
   // exception from the clone() method but aren't allowed to due to clone()'s
   // signature in java.lang.Object.  So boy: Never ever in the future use
   // one of java.lang.Object's methods for Application purposes!
    return this;
  }



  /**
   * Convert this <code>FirstClassObject</code> into its corresponding raw
   * Java representation.  If the object represents a
   * <code>SchemeDouble</code> this method would return an instance of
   * <code>java.lang.Double</code>.
   *
   * @return The corresponding object from the Java type system.
   */
  public abstract Object convertToJava();



  /**
   * Get the symbolic typename.  Handles NIL references.
   *
   * @param o The object for which the typename gets computed.
   * @return The instances type name.
   * @see FirstClassObject#getTypename()
   */
  public static String getTypename( FirstClassObject o )
  {
    if ( o == Cons.NIL )
      return "NIL";
    else
      return o.getTypename();
  }



  /**
   * Return the typename of this <code>FirstClassObject</code> instance.  This
   * is one of:<br>
   * <code>
   *  list
   *  symbol
   *  vector
   *  integer
   *  real
   *  string
   *  char
   *  object
   * </code>
   * The default implementation returns the value of the public final static
   * TYPE_NAME field. If this does not exist or is not accessible the name
   * of the class is returned.
   *
   * @return The instances type name.
   */
  public String getTypename()
  {
    String result = null;

    try
    {
      result = (String)getClass().getField( "TYPE_NAME" ).get( this );
    }
    catch ( Throwable t )
    {
      result = getClass().getName();
    }

    return result;
  }



  /**
   * Mark a <code>FirstClassObject</code> as constant.  Note that it is not
   * possible to switch from constant to variable, only the state change from
   * variable to constant is allowed.  Handles NIL values.
   *
   * @param fco The <code>FirstClassObject</code> to modify.
   * @param what The new value.
   * @throws IllegalArgumentException In case it has been tried to change the
   *         state of the <code>FirstClassObject</code> from constant to
   *         variable.
   */
  static void setConstant( FirstClassObject fco, boolean what )
  {
    if ( Cons.NIL != fco )
      fco.setConstant( what );
  }



  /**
   * Can be used to mark a <code>FirstClassObject</code> as constant.  Note
   * that it is not possible to switch from constant to variable, only the
   * state change from variable to constant is allowed.
   *
   * @param what The new value.
   * @throws IllegalArgumentException In case it has been tried to change the
   *         state of the <code>FirstClassObject</code> from constant to
   *         variable.
   */
  void setConstant( boolean what )
  {
    if ( _isConstant && !what )
      throw new IllegalArgumentException(
        "Constant->variable change not allowed." );

    _isConstant = what;
  }



  /**
   * Returns whether an <code>FirstClassObject</code> is a constant. Constant
   * objects are the self-evaluating primitves and can be created by the
   * <code>quote</code> syntax.
   *
   * @param fco The <code>FirstClassObject</code> to test.
   * @return Whether this should be seen as a constant.
   */
  static boolean isConstant( FirstClassObject fco )
  {
   // I like my C history...
   return fco == Cons.NIL || fco.isConstant();
  }



  /**
   * Returns whether an <code>FirstClassObject</code> is a constant. Constant
   * objects are the self-evaluating primitves and can be created by the
   * <code>quote</code> syntax.
   *
   * @return Whether this should be seen as a constant.
   */
  public boolean isConstant()
  {
    return _isConstant;
  }



  /**
   * Compile this <code>FirstClassObject</code>.  This default implementation
   * just returns identity.
   *
   * @param environment The compile environment.
   * @return The compiled object.
   * @throws RuntimeX In case of compile errors.
   * @see #compile( FirstClassObject, Environment )
   */
  FirstClassObject compile( Environment environment )
    throws RuntimeX
  {
    return this;
  }



  /**
   * Compile the passed first class object.  This default implementation just
   * returns identity.  Handles <code>NIL</code> references.  The passed
   * environment is used to look up the needed definitions for compilation.
   *
   * @param fco The object to compile.
   * @param env The compile environment
   * @return The compiled object.
   * @throws RuntimeX In case of compile errors.
   */
  public static FirstClassObject compile( FirstClassObject fco, Environment env )
    throws RuntimeX
  {
    FirstClassObject result = fco;

    if ( fco != Cons.NIL )
      result = fco.compile( env );
    return result;
  }
}
