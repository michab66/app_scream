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
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.Symbol;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;
import de.michab.scream.util.Scut;

public class SyntaxCond extends Operation
{
    static Symbol ELSE = Symbol.createObject( "else" );

    private SyntaxCond()
    {
        super( "cond" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 1, Integer.MAX_VALUE, args );

        for ( Cons c = args ; c != Cons.NIL ; c = Scut.as( Cons.class, c.getCdr() ) )
        {
            var fco = c.getCar();
            if ( ! (fco instanceof Cons) )
                throw new RuntimeX( Code.BAD_CLAUSE,
                        toString( fco ) );
            Cons clause = Scut.as( Cons.class, fco);

            // TODO unexpected ELSE message.
            if ( eqv( ELSE, clause.getCar() ) )
            {
                if ( Cons.NIL != c.getCdr() )
                    throw new RuntimeX( Code.BAD_CLAUSE,
                            toString( fco ) );
                clause.setCar( SchemeBoolean.T );
            }
        }

        L l = (e,c) -> _cond(
                e,
                args,
                c);

        return new Lambda( l, getName() );
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

            return Continuation._begin( e, (Cons)clause.getCdr(), trueBranch );
        };

        return Continuation._eval(
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
     *
     */
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
        tle.setPrimitive( new SyntaxCond() );

        return tle;
    }
};

