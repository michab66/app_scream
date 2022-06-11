/* $Id: ShortcutAnd.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */

package de.michab.scream.pops;

import de.michab.scream.*;



/**
 */
public class ShortcutAnd
  extends Syntax
{
  /**
   *
   */
  private final FirstClassObject[] _seq;


  /**
   * Create an 'ShortcutAnd' primitive operation from the passed expression
   * sequence.
   *
   * @param seq The expression sequence.
   */
  public ShortcutAnd( FirstClassObject[] seq )
  {
    super( "popShortcutAnd" );
    _seq = seq;
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
      if ( _seq.length == 0 )
        return SchemeBoolean.T;

      for ( int i = 0 ; i < _seq.length -1 ; i++ )
      {
        FirstClassObject result = evaluate( _seq[i], p );
        if ( SchemeBoolean.F == result )
          return SchemeBoolean.F;
      }

      return evaluateTrailingContext( _seq[ _seq.length-1 ], p );
  }
}
