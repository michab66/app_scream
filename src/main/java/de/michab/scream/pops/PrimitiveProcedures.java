/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 *
 */
public abstract class PrimitiveProcedures
{
    private PrimitiveProcedures()
    {
        throw new AssertionError();
    }

    static private Procedure apply( Environment e )
    {
        return new Procedure( "scream:apply", e )
        {
            /**
             * <pre>
             * (define (make-argument-list list)
             *   (let ((first (car list)) (rest (cdr list)))
             *     (if (null? rest)
             *       first
             *       (cons first (make-argument-list rest)))))
             * </pre>
             */
            private Cons makeArgumentList( Cons list ) throws RuntimeX
            {
                var first = list.getCar();
                var rest = Scut.as( Cons.class, list.getCdr() );

                if ( Cons.NIL == rest )
                    return Scut.as( Cons.class, first );

                return new Cons( first, makeArgumentList( rest ) );
            }

            @Override
            protected Thunk _executeImpl(
                    Environment e,
                    Cons args,
                    Cont<FirstClassObject> c )
                            throws RuntimeX
            {
                checkArgumentCount( 1, Integer.MAX_VALUE, args );

                Procedure proc = Scut.as(
                        Procedure.class,
                        args.listRef( 0 ) );
                var list = Scut.as(
                        Cons.class,
                        args.getCdr() );

                return proc.apply(
                        makeArgumentList( list ),
                        c );
            }
        };
    }

    static private Procedure car( Environment e )
    {
        return new Procedure( "scream:car", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );
                var firstArgument = Scut.assertType(
                        Cons.class,
                        args.getCar(),
                        Symbol.createObject( "car" ),
                        1 );
                return c.accept( firstArgument.getCar() );
            }
        };
    }

    static private Procedure cdr( Environment e )
    {
        return new Procedure( "scream:cdr", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );
                var firstArgument = Scut.assertType(
                        Cons.class,
                        args.getCar(),
                        Symbol.createObject( "cdr" ),
                        1 );
                return c.accept( firstArgument.getCdr() );
            }
        };
    }

    static private Procedure eq = new Procedure( "scream:eq?", null )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 2, args );
            var arguments = args.asArray();
            return c.accept(
                    Bool.createObject(
                            FirstClassObject.eq( arguments[0], arguments[1] ) ) );
        }
    };

    static private Procedure eqv = new Procedure( "scream:eqv?", null )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 2, args );
            var arguments = args.asArray();
            return c.accept(
                    Bool.createObject(
                            FirstClassObject.eqv( arguments[0], arguments[1] ) ) );
        }
    };

    static private Procedure equal = new Procedure( "scream:equal?", null )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 2, args );
            var arguments = args.asArray();
            return c.accept(
                    Bool.createObject(
                            FirstClassObject.equal( arguments[0], arguments[1] ) ) );
        }
    };

    static private Procedure nullq( Environment e )
    {
        return new Procedure( "scream:null?", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );
                return c.accept(
                        Bool.createObject( Cons.NIL == args.getCar() ) );
            }
        };
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( apply( tle ) );
        tle.setPrimitive( car( tle ) );
        tle.setPrimitive( cdr( tle ) );
        tle.setPrimitive( eq );
        tle.setPrimitive( eqv );
        tle.setPrimitive( equal );
        tle.setPrimitive( nullq( tle ) );

        return tle;
    }
}
