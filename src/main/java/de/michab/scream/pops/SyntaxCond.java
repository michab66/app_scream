/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code (cond <clause₁> <clause₂> ...)} syntax
 * <p>
 * {@code r7rs 4.2.1}
 */
public class SyntaxCond extends Syntax
{
    static Symbol ELSE = Symbol.createObject( "else" );

    private SyntaxCond()
    {
        super( "cond" );
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, Integer.MAX_VALUE, args );

        for ( Cons arg = args ; arg != Cons.NIL ; arg = Scut.as( Cons.class, arg.getCdr() ) )
        {
            var fco = arg.getCar();
            if ( ! (fco instanceof Cons) )
                throw RuntimeX.mBadClause( fco );
            Cons clause = Scut.as( Cons.class, fco);

            // TODO unexpected ELSE message.
            if ( eqv( ELSE, clause.getCar() ) )
            {
                if ( Cons.NIL != arg.getCdr() )
                    throw RuntimeX.mBadClause( fco );
                clause.setCar( SchemeBoolean.T );
            }
        }

        return _cond(
                e,
                args,
                c);
    }

    private static Thunk _clause(
            Environment e,
            Cons clause,
            Cont<FirstClassObject> c,
            Thunk falseBranch)
                    throws RuntimeX
    {
        Cont<FirstClassObject> next = s -> {
            if ( ! SchemeBoolean.isTrue( s ) )
                return falseBranch;

            Cons afterTest = (Cons)clause.getCdr();

            // rsr7: If the selected <clause> contains only the <test> and no
            // <expression>s, then the value of the <test> is returned as
            // the result.
            if ( Cons.NIL == afterTest )
                return c.accept( s );

            return Primitives._begin( e, (Cons)clause.getCdr(), c );
        };

        return Primitives._eval(
                e,
                clause.getCar(),
                next );
    }

    private static Thunk _cond(
            Environment e,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        Thunk next = () -> _cond(
                e,
                (Cons)clauses.getCdr(),
                c );

        return _clause(
                e,
                (Cons)clauses.getCar(),
                c,
                next );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendNullEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( new SyntaxCond() );

        return tle;
    }
};

