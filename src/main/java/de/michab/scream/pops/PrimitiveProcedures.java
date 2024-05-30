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
import de.michab.scream.util.FunctionX;
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

                return new Cons(
                        first,
                        makeArgumentList( rest ) );
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

    private static class CadrSupport extends Procedure
    {
        private final FunctionX<Cons,FirstClassObject,RuntimeX> _f;
        private final Symbol _name;

        public CadrSupport( String name, FunctionX<Cons,FirstClassObject,RuntimeX> f )
        {
            super( "scream:" + name, null );

            _f = f;
            _name = Symbol.createObject( name );
        }

        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );
            var firstArgument = Scut.assertType(
                    Cons.class,
                    args.getCar(),
                    _name,
                    1 );

            return c.accept(
                    _f.apply( firstArgument ) );
        }
    }

    static private Procedure car =
            new CadrSupport( "car", cons -> { return cons.getCar(); } );

    static private Procedure cdr =
            new CadrSupport( "cdr", cons -> { return cons.getCdr(); } );

    @FunctionalInterface
    public interface BiFunctionX<T1, T2, R, X extends Exception> {

        /**
         * Applies this function to the given arguments.
         *
         * @param t the first function argument
         * @return the function result
         */
        R apply(T1 t1, T2 t2)
            throws X;
    }

    private static class EquivalenceSupport extends Procedure
    {
        private final BiFunctionX<FirstClassObject, FirstClassObject, Boolean, RuntimeX> _f;

        public EquivalenceSupport( String name, BiFunctionX<FirstClassObject, FirstClassObject, Boolean, RuntimeX> f )
        {
            super( name, null );

            _f = f;
        }

        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 2, args );
            var arguments = args.asArray();
            return c.accept(
                    Bool.createObject(
                            _f.apply( arguments[0], arguments[1] ) ) );
        }
    }

    static private Procedure eq =
            new EquivalenceSupport( "scream:eq?", FirstClassObject::eq );

    static private Procedure eqv =
            new EquivalenceSupport( "scream:eqv?", FirstClassObject::eqv );

    static private Procedure equal =
            new EquivalenceSupport( "scream:equal?", FirstClassObject::equal );

    static private Procedure nullq = new Procedure( "scream:null?", null )
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
        tle.setPrimitive( car );
        tle.setPrimitive( cdr );
        tle.setPrimitive( eq );
        tle.setPrimitive( eqv );
        tle.setPrimitive( equal );
        tle.setPrimitive( nullq );

        return tle;
    }
}
