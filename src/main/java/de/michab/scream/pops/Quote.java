/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream.pops;

import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.Syntax;
import de.michab.scream.util.BiFunctionX;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;

/**
 * The implementation of the primitive {@code quote} operation.  This is
 * the result of an compile operation.  Experimental state.
 */
public class Quote
    extends Syntax
{
    private final BiFunctionX<Environment, Cont<FirstClassObject>, Thunk> thunk;

    /**
     * The quoted object.
     */
    private FirstClassObject _quoted;

    /**
     * Create a 'quote' primitive operation.
     */
    public Quote( FirstClassObject quoted )
    {
        super( "popQuote" );
        _quoted = quoted;

        thunk = (e,c) -> c.accept( quoted );
    }

    /**
     * Pops must not have an activate since everything that the activate did has
     * been done at compile time.  Instead they have to implement the evaluate.
     * Background here is that the border between evaluate and activate in the
     * past has been crossed in the Cons evaluate.  But one result of compilation
     * is the replacement of the complete cons by the compiled operation that can
     * then be simply evaluated.
     */
    @Override
    public FirstClassObject evaluate( Environment parent )
            throws RuntimeX
    {
        return _quoted;
    }
    @Override
    public Thunk evaluate( Environment e, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        return () -> thunk.apply( e, c );
    }
}
