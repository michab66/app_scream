/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
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
import de.michab.scream.SchemeBoolean;

/**
 * (or <test1> ... ) syntax; r7rs p15
 *
 * The test expressions are evaluated from left to right, and the value of
 * the first expression that evaluates to a true value (see section 6.3.1) is
 * returned. Any remaining expressions are not evaluated. If all expressions
 * evaluate to false values, the value of the last expression is returned. If
 * there are no expressions then #f is returned.
 */
public class SyntaxOr extends Operation
{
    private SyntaxOr()
    {
        super( "or" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 0, Integer.MAX_VALUE, args );

        L l = (e,c) -> _or(
                e,
                args,
                c);

        return new Lambda( l, getName() );
    }

    /**
     * Shortcut or.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    private static Thunk _or(
            Environment e,
            Cons expressions,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        if ( expressions == Cons.NIL )
            return () -> c.accept( previousResult );
        if ( SchemeBoolean.isTrue( previousResult ) )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _or( e, (Cons)expressions.getCdr(), fco, c);

        return () -> Continuation._eval(
                e,
                expressions.getCar(),
                next );
    }

    /**
     * Shortcut or.
     *
     * @param e The environment for evaluation.
     * @param expressions The list of tests.
     * @param previousResult The value of the previous test.
     * @param c The continuation.
     * @return A thunk.
     * @throws RuntimeX In case of an error.
     */
    public static Thunk _or(
            Environment e,
            Cons expressions,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return _or(
                e,
                expressions,
                SchemeBoolean.F,
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

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxOr() );

        return tle;
    }
}
