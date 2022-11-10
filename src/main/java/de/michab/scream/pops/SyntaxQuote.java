package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;
import urschleim.Holder;

/**
 * Switch off evaluation for the single passed argument.
 *
 * (quote <datum>) syntax; r5rs 8
 */
public class SyntaxQuote extends Operation
{
    private SyntaxQuote()
    {
        super( "quote" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var quoted = args.getCar();

        return new Lambda(
                (e,c) -> Continuation._quote(
                        e,
                        quoted,
                        c ),
                this.toString() );
    }

    @Override
    public FirstClassObject compile( Environment parent, Cons args )
            throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var quoted = args.getCar();

        Lambda.L result = (e,c) -> c.accept( quoted );

        return new Lambda( result, getName().toString() );
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
        checkArgumentCount( 1, args );
        // setConstant?
        return c.accept( args.getCar() );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxQuote() );

        return tle;
    }
}
