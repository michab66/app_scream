/* $Id: Quote.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>quote</code> operation.  This is
 * the result of an compile operation.  Experimental state.
 */
public class Quote
  extends Syntax
{
  /**
   * The quoted stuff.
   */
  private FirstClassObject _quoted;



  /**
   * Create a 'quote' primitive operation.
   */
  public Quote( FirstClassObject quoted )
  {
    super( "popQuote" );
    _quoted = quoted;
  }



  /**
   * Pops must not have an activate since everything that the activate did has
   * been done at compile time.  Instead they have to implement the evaluate.
   * Background here is that the border between evaluate and activate in the
   * past has been crossed in the Cons evaluate.  But one result of compilation
   * is the replacement of the complete cons by the compiled operation that can
   * then be simply evaluated.
   */
  public FirstClassObject evaluate( Environment parent )
    throws RuntimeX
  {
    return _quoted;
  }
}
