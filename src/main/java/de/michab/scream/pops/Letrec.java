/* $Id: Letrec.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.*;



/**
 * The implementation of the primitive <code>letrec</code> operation.  This is
 * the result of an compile operation.  Experimental state.
 */
public class Letrec
  extends Syntax
{
  /**
   *
   */
  private static final Symbol LETREC_UNDEFINED =
    Symbol.createObject( "@undefined" );


  /**
   *
   */
  private final Symbol[] _variables;

  /**
   *
   */
  private final FirstClassObject[] _inits;

  /**
   *
   */
  private final Sequence _body;



  /**
   *
   */
  public Letrec( Symbol[] vars,
                      FirstClassObject[] inits,
                      FirstClassObject[] body )
  {
    super( "popLet*" );
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

    for ( int i = _variables.length-1 ; i >= 0 ; i-- )
      nested.define( _variables[i], LETREC_UNDEFINED );

    // Do the bindings.  Backwards iteration for elegance and speed.
    for ( int i = 0 ; i < _variables.length ; i++ )
      nested.assign( _variables[i], evaluate( _inits[i], nested ) );

    // At last evaluate the <body> in our hand-made environment
    // and return the result.
    return evaluate( _body, nested );
  }
}
