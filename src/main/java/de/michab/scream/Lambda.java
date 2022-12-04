/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import urschleim.Holder;

/**
 * A lambda encapsulation for an fco.
 *
 * @author micbinz
 */
public class Lambda extends FirstClassObject
{
    @FunctionalInterface
    public static interface L {
        Thunk accept(Environment e, Cont<FirstClassObject> c) throws RuntimeX;
    }

    /**
     * The actual lambda closure.
     */
    private final L _l;

    /**
     * A name.
     */
    private final String _name;

    /**
     * Context info for debugging.
     */
    private Cons _info;

    public Lambda( L l, String name )
    {
        _l = l;
        _name = name;
    }
    public Lambda( L l, Symbol name )
    {
        this( l, name.toString() );
    }
    public Lambda( L l )
    {
        this( l, "anonymous" );
    }

    @Override
    @Deprecated
    protected FirstClassObject evaluate( Environment e ) throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline( evaluate(
                e,
                Continuation.endCall( r::set ) ),
                error::set );

        if ( error.get() != null )
            throw (RuntimeX)error.get();

       return r.get();
    }
    @Override
    public Thunk evaluate( Environment e, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        return _l.accept( e, c );
    }

    @Override
    public String toString()
    {
        return  String.format(
                "<%s %s id=%d info=\"%s\">",
                typename(),
                _name,
                id(),
                _info );
    }

    @Override
    public Object toJava() throws RuntimeX
    {
        return Void.TYPE;
    }

    public Lambda setInfo( Cons c )
    {
        _info = c;
        return this;
    }
}
