/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;

public class SyntaxCond extends Syntax
{
    static Symbol ELSE = Symbol.createObject( "else" );

    private SyntaxCond()
    {
        super( "cond" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, Integer.MAX_VALUE, args );

        for ( Cons arg = args ; arg != Cons.NIL ; arg = Scut.as( Cons.class, arg.getCdr() ) )
        {
            var fco = arg.getCar();
            if ( ! (fco instanceof Cons) )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( fco ) );
            Cons clause = Scut.as( Cons.class, fco);

            // TODO unexpected ELSE message.
            if ( eqv( ELSE, clause.getCar() ) )
            {
                if ( Cons.NIL != arg.getCdr() )
                    throw new RuntimeX( Code.BAD_CLAUSE,
                            toString( fco ) );
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
            Cont<FirstClassObject> trueBranch,
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
                return trueBranch.accept( s );

            return Primitives._x_begin( e, (Cons)clause.getCdr(), trueBranch );
        };

        return Primitives._x_eval(
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
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxCond() );

        return tle;
    }
};

