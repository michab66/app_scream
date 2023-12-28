/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import java.util.HashSet;

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
 * {@code (case <key> <clause₁> <clause₂> ...)} syntax
 * <p>
 * {@code r7rs 4.2.1 p15}
 */
public class SyntaxCase extends Syntax
{
    static Symbol ELSE = Symbol.createObject( "else" );

    SyntaxCase()
    {
        super( "case" );
    }

    /**
     * Validate a single clause.
     *
     * @param unifier
     * @param clause
     * @throws RuntimeX
     */
    private void validateClause( HashSet<FirstClassObject> unifier, final Cons clause )
            throws RuntimeX
    {
        // datum exp1 ...
        checkArgumentCount( 2, Integer.MAX_VALUE, clause );

        final var datum = clause.getCar();

        if ( FirstClassObject.equal( ELSE, datum ) )
        {
            // must be last clause.
            if ( Cons.NIL != clause.getCdr() )
                throw RuntimeX.mBadClause( clause.getCdr() );
        }
        else
        {
            Cons cdatum = Scut.as(
                    Cons.class,
                    datum,
                    s -> { throw RuntimeX.mBadClause( clause ); });
            // datum must be at least a one element list.
            checkArgumentCount(
                    1,
                    Integer.MAX_VALUE,
                    cdatum );
            Scut.assertUnique(
                    unifier,
                    cdatum );
        }
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var key = args.getCar();
        var clauses = Scut.as( Cons.class, args.getCdr() );

        var unifier = new HashSet<FirstClassObject>();

        // Validate clauses
        for ( var i = clauses ; i != Cons.NIL ; i = Scut.as( Cons.class, i.getCdr() ) )
        {
            final var fi = i;
            final var ct = Scut.as(
                    Cons.class,
                    i.getCar(),
                    s -> { throw RuntimeX.mBadClause( fi ); } );

            validateClause( unifier, ct );
        }

        return _case(
                e,
                key,
                clauses,
                c);
    }

    /**
     *
     * @param e
     * @param key the evaluated key.
     * @param clauses The remaining clauses.  May be nil.
     * @param datums The datum list of the current clause. nil if no clauses remain.
     * @param body the expressions of the current clause.
     * @param c
     * @return
     * @throws RuntimeX
     */
    private static Thunk _caseImpl(
            Environment e,
            FirstClassObject key,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        var currentClause = Scut.as( Cons.class, clauses.getCar() );

        if ( Symbol.createObject( "else" ).equals( currentClause.getCar() ))
        {
            return Primitives._x_begin(
                    e,
                    Scut.as( Cons.class, currentClause.getCdr() ),
                    c );
        }
        var datums = Scut.as( Cons.class, currentClause.getCar() );
        if ( SchemeBoolean.isTrue( datums.member( key ) ) )
        {
            return Primitives._x_begin(
                    e,
                    Scut.as( Cons.class, currentClause.getCdr() ),
                    c );
        }

        return _caseImpl(
                e,
                key,
                (Cons)clauses.getCdr(),
                c );
    }

    private static Thunk _thunked_caseImpl(
            Environment e,
            FirstClassObject key,
            Cons clauses,
            Cont<FirstClassObject> c)
    {
        return () -> _caseImpl( e, key, clauses, c );
    }

    private static Thunk _case(
            Environment e,
            FirstClassObject key,
            Cons clauses,
            Cont<FirstClassObject> c) throws RuntimeX
    {
        if ( Cons.NIL == clauses )
            return c.accept( Cons.NIL );

        Cont<FirstClassObject> next = result -> _thunked_caseImpl(
                e,
                result,
                clauses,
                c );

        return Primitives._x_eval(
                e,
                key,
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
        tle.setPrimitive( new SyntaxCase() );

        return tle;
    }
}
