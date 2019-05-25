/* $Id: Sequence.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */

package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>begin</code> operation.  This is
 * the result of an compile operation.  Experimental state.
 */
public class Sequence
  extends Syntax
{
  /**
   * The actual sequence.
   */
  private FirstClassObject[] _sequence;



  /**
   * Create a 'sequence' primitive operation.
   */
  public Sequence( FirstClassObject[] seq )
  {
    super( "popSequence" );
    _sequence = seq;
  }



  /**
   * Executes the compiled syntax.
   *
   * @param parent The execution environment.
   * @return The result of the syntax execution.
   * @throws RuntimeX In case of an execution error.
   */
  public FirstClassObject evaluate( Environment parent )
    throws RuntimeX
  {
    return interpretTailSequence( _sequence, parent );
  }
}
