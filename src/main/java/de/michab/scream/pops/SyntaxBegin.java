package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;

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
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        L l = (e,c) -> _begin(
                e,
                args,
                c);

        return new Lambda( l, getName() );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param previousResult The result of the previous expression.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    private static Thunk _begin(
            Environment e,
            Cons body,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        if ( body == Cons.NIL )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _begin( e, (Cons)body.getCdr(), fco, c);

        return () -> Continuation._eval( e, body.getCar(), next );
    }

    /**
     * Evaluate a list of expressions and return the value of the final element.
     * @param e The environment for evaluation.
     * @param body A list of expressions.
     * @param previousResult The result of the previous expression.
     * @param c The continuation receiving the result.
     * @return The thunk.
     */
    public static Thunk _begin(
            Environment e,
            Cons body,
            Cont<FirstClassObject> c )
    {
        return _begin(
                e,
                body,
                Cons.NIL,
                c );
    }

    @Override
    public FirstClassObject compile( Environment parent, Cons args )
            throws RuntimeX
    {
        return _compile( parent, args );
    }
    @Override
    public FirstClassObject activate( Environment parent,
            Cons arguments )
                    throws RuntimeX
    {
        var λ = _compile( parent, arguments );

        return FirstClassObject.evaluate( λ, parent );
    }

    @Override
    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {

        return _begin( e, args, c );
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
