package de.michab.scream;

import urschleim.Continuation;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;
import urschleim.Holder;

/**
 *
 * @author micbinz
 */
public class Lambda extends FirstClassObject
{
    @FunctionalInterface
    public static interface L {
        Thunk accept(Environment e, Cont<FirstClassObject> c) throws RuntimeX;
    }

    private final L _l;
    private final String _name;

    public Lambda( L l, String name )
    {
        _l = l;
        _name = name;
    }
    public Lambda( L l, Symbol name )
    {
        _l = l;
        _name = name.toString();
    }
    public Lambda( L l )
    {
        this( l, "anonymous" );
    }

    @Override
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
                "<%s %s>",
                typename(),
                _name );
    }

    @Override
    public Object toJava() throws RuntimeX
    {
        return Void.TYPE;
    }
}
