/* $Id: ShortcutOr.java 788 2015-01-10 23:07:11Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */

package de.michab.scream.pops;

import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.Syntax;



/**
 */
public class ShortcutOr
  extends Syntax
{
  /**
   *
   */
  private final FirstClassObject[] _seq;


  /**
   * Create an 'ShortcutOr' primitive operation from the passed expression
   * sequence.
   *
   * @param seq The expression sequence.
   */
  public ShortcutOr( FirstClassObject[] seq )
  {
    super( "popShortCutOr" );
    _seq = seq;
  }



  /**
   * Executes the compiled syntax.
   *
   * @param p The execution environment.
   * @return The result of the syntax execution.
   * @throws RuntimeX In case of an execution error.
   */
  @Override
public FirstClassObject evaluate( Environment p )
    throws RuntimeX
  {
      if ( _seq.length == 0 )
        return SchemeBoolean.F;

      // Evaluate all but the last expression in the list.
      for ( int i = 0 ; i < _seq.length-1 ; i++ )
      {
        FirstClassObject result = evaluate( _seq[i], p );
        // If one of the expressions evaluates to true which is everything but
        // false...
        if ( result != SchemeBoolean.F )
          // ...we do a shortcut return.
          return result;
      }

      // Evaluate the last expression in a trailing context.
      return evaluateTrailingContext( _seq[ _seq.length-1 ], p );
  }
}
