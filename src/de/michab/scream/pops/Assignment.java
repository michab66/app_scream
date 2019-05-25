/* $Id: Assignment.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */

package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;




/**
 * Assignment primitive.
 */
public class Assignment
  extends Syntax
{
  /**
   * The symbol to assign.
   */
  private final Symbol _symbol;



  /**
   * The expression for the assignment.
   */
  private final FirstClassObject _value;



  /**
   * Create an 'Assignment' primitive operation from the passed expression
   * sequence.
   *
   * @param symbol The symbol to assign.
   * @param value The value to assign.
   */
  public Assignment( Symbol symbol, FirstClassObject value )
  {
    super( "popAssignment" );
    _symbol = symbol;
    _value = value;
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
    // Perform the assignment.
    p.assign( _symbol, evaluate( _value, p ) );
    // This is unspecified, so let's do it like PCS3.
    return Cons.NIL;
  }
}
