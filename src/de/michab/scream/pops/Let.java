/* $Id: Let.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>let</code> operation.  This is the
 * result of an compile operation.  Experimental state.
 */
public class Let
  extends Syntax
{
  private final Symbol[] _variables;

  private final FirstClassObject[] _inits;

  private final Sequence _body;



  /**
   *
   */
  public Let( Symbol[] vars, FirstClassObject[] inits, FirstClassObject[] body )
  {
    super( "popLet" );
    _variables = vars;
    _inits = inits;
    _body = new Sequence( body );
  }



  /**
   *
   */
  public FirstClassObject evaluate( Environment p )
    throws RuntimeX
  {
    Environment nested = p.extend();

    // Do the bindings.  Backwards iteration for elegance and speed.
    for ( int i = _variables.length -1 ; i >= 0 ; i-- )
      nested.set( _variables[i], evaluate( _inits[i], p ) );

    // At last evaluate the <body> in our hand-made environment
    // and return the result.
    return evaluate( _body, nested );
  }
}
