/* $Id: Loop.java 185 2009-06-21 12:30:22Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>loop</code> operation.  This is
 * the result of an compile operation.  Experimental state.
 */
public class Loop
  extends Syntax
{
  private final Symbol[] _variables;

  private final FirstClassObject[] _inits;
  private final FirstClassObject[] _steps;

  private final FirstClassObject _test;
  private final FirstClassObject[] _expression;

  private final FirstClassObject[] _commands;



  /**
   * Create a 'loop' primitive.
   *
   * @param vars
   * @param inits
   * @param steps
   * @param test
   * @param expression
   * @param commands
   */
  public Loop( Symbol[] vars,
               FirstClassObject[] inits,
               FirstClassObject[] steps,
               FirstClassObject test,
               FirstClassObject[] expression,
               FirstClassObject[] commands )
  {
    super( "popLoop" );

    _variables = vars;
    _inits = inits;
    _steps = steps;
    _test = test;
    _expression = expression;
    _commands = commands;
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
    Environment nested = p.extend();

    // Init phase.  Create the bindings.  Backwards iteration for elegance and
    // speed.
    for ( int i = _variables.length -1 ; i >= 0 ; i-- )
      nested.set( _variables[i], evaluate( _inits[i], p ) );

    // Actual loop execution.
    while ( SchemeBoolean.F == evaluate( _test, nested ) )
    {
      // Process the loop's content.
      for ( int i = 0 ; i < _commands.length ; i++ )
        evaluate( _commands[i], nested );

      // Process step expressions.
      for ( int i = _variables.length -1 ; i >= 0 ; i-- )
        nested.assign( _variables[i], evaluate( _steps[i], nested ) );
    }

    // Loop termination.
    if ( _expression.length == 0 )
      return Cons.NIL;
    else
      return interpretTailSequence( _expression, nested );
  }
}
