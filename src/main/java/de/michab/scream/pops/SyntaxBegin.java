package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException;
import urschleim.Continuation;
import urschleim.Continuation.Cont;
import urschleim.Continuation.Thunk;
import urschleim.Holder;

/**
 * (begin exp1 exp2 ...) library syntax; r7rs 17
 */
public class SyntaxBegin extends Operation
{
    private SyntaxBegin()
    {
        super( "begin" );

    }

    @Override
    public FirstClassObject compile( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        throw new InternalError();
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        L l = (e,c) -> Continuation._begin(
                e,
                args,
                c);

        return new Lambda( l, getName() );
    }

    @Override
    public FirstClassObject activate( Environment e, Cons argumentList )
            throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline( _activate(
                e,
                argumentList,
                Continuation.endCall( r::set ) ),
                error::set );

        if ( error.get() != null )
            throw (RuntimeX)error.get();

       return r.get();
    }

    @Override
    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {

        return Continuation._begin( e, args, c );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxBegin() );

        return tle;
    }
}
