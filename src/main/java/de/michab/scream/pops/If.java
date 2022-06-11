/* $Id: If.java 185 2009-06-21 12:30:22Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>if</code> operation.  This is the
 * result of an compile operation.  Experimental state.
 */
public class If
  extends Syntax
{
  /**
   * The if's condition.
   */
  private final FirstClassObject _condition;


  /**
   * The expression to evaluate if condition evaluates to true.
   */
  private final FirstClassObject _onTrue;



  /**
   * The expression to evaluate if condition evaluates to false (the 'else'
   * branch).
   */
  private final FirstClassObject _onFalse;



  /**
   * Create an 'if' primitive operation with both conditions.
   *
   * @param condition The condition expression.
   * @param onTrue The expression to evaluate in case the condition evaluates
   *               to a result different from <code>#F</code>.
   * @param onFalse The expression to evaluate in case the condition evaluates
   *               to <code>#F</code>.
   */
  public If( FirstClassObject condition,
             FirstClassObject onTrue,
             FirstClassObject onFalse )
  {
    super( "popIf" );

    _condition = condition;
    _onTrue = onTrue;
    _onFalse = onFalse;
  }



  /**
   * Executes the compiled syntax.
   *
   * @param p The execution environment.
   * @return The result of the syntax execution.
   * @throws RuntimeX In case of an execution error.
   */
  public FirstClassObject evaluate( Environment p )
    throws RuntimeX
  {
    FirstClassObject cond = evaluate( _condition, p );

    if ( cond == SchemeBoolean.F )
      return evaluateTrailingContext( _onFalse, p );
    else
      return evaluateTrailingContext( _onTrue, p );
  }
}
