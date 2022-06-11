/* $Id: Cond.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>cond</code> operation.  This is
 * the result of a compile operation.<br>
 * This primitive is centered around an expression map that looks like <code>
 * <br>
 * +-------------+--------+-----+<br>
 * | condition 1 | expr 1 | ... |<br>
 * +-------------+--------+-----+-----+<br>
 * | condition 2 | expr 1 | ... | ... |<br>
 * +-------------+--------+-----+-----+<br>
 * | ...         | expr 1 | ... |<br>
 * +-------------+--------+-----+-----+-----+<br>
 * | condition n | expr 1 | ... | ... | ... |<br>
 * +-------------+--------+-----+-----+-----+<br>
 * <br></code>
 * The conditions are evaluated from 1 to n.  As soon a condition evaluates
 * to true the expression sequence gets evaluated from expr1 to n and the
 * result of the last one is returned.
 */
public class Cond
  extends Syntax
{
  /**
   * The clause map.
   */
  private final FirstClassObject[][] _clauseMap;



  /**
   * Creates the primitive based on the expression map.
   *
   * @param clauses The expression map.
   */
  public Cond( FirstClassObject[][] clauses )
  {
    super( "popCond" );
    _clauseMap = clauses;
  }



  /**
   * Executes the compiled sytax.
   *
   * @param p The execution environment.
   * @return The result of the syntax execution.
   * @throws RuntimeX In case of an execution error.
   */
  public FirstClassObject evaluate( Environment p )
    throws RuntimeX
  {
    for ( int i = 0 ; i < _clauseMap.length ; i++ )
    {
      FirstClassObject cond = evaluate( _clauseMap[i][0], p );
      if ( cond != SchemeBoolean.F )
      {
        return ( _clauseMap[i].length > 1 ) ?
          interpretTailSequence( _clauseMap[i], 1, p ) :
          cond;
      }
    }

    return Cons.NIL;
  }
}
