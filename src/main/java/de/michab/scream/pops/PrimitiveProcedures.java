/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Bytevector;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.PortIn;
import de.michab.scream.fcos.PortInBinary;
import de.michab.scream.fcos.PortOut;
import de.michab.scream.fcos.PortOutBinary;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Real;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Vector;
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

    static private Procedure binaryInputPortQ =
            new TypePredicate( "scream:binary-input-port?", PortInBinary.class );

    static private Procedure binaryOutputPortQ =
            new TypePredicate( "scream:binary-output-port?", PortOutBinary.class );

    static private Procedure booleanQ =
            new TypePredicate( "scream:boolean?", Bool.class );

    static private Procedure bytevectorQ =
            new TypePredicate( "scream:bytevector?", Bytevector.class );

    static private Procedure car =
            new CadrSupport( "car", cons -> { return cons.getCar(); } );

    static private Procedure cdr =
            new CadrSupport( "cdr", cons -> { return cons.getCdr(); } );

    static private Procedure charQ =
            new TypePredicate( "scream:char?", SchemeCharacter.class );

    static private Procedure consQ =
            new TypePredicate( "scream:cons?", Cons.class );

    static private Procedure eq =
            new EquivalenceSupport( "scream:eq?", FirstClassObject::eq );

    static private Procedure eqv =
            new EquivalenceSupport( "scream:eqv?", FirstClassObject::eqv );

    static private Procedure equal =
            new EquivalenceSupport( "scream:equal?", FirstClassObject::equal );

    static private Procedure gtq =
            new NumberComparisonSupport( ">", (a,b) -> { return a.r7rsGreaterThan( b ); } );

    static private Procedure gteq =
            new NumberComparisonSupport( ">=", (a,b) -> { return a.r7rsGreaterOrEqualThan( b ); } );

    static private Procedure inputPortQ =
            new TypePredicate( "scream:input-port?", PortIn.class );

    static private Procedure integerQ =
            new TypePredicate( "scream:integer?", Int.class );

    static private Procedure ltq =
            new NumberComparisonSupport( "<", (a,b) -> { return a.r7rsLessThan( b ); } );

    static private Procedure lteq =
            new NumberComparisonSupport( "<=", (a,b) -> { return a.r7rsLessOrEqualThan( b ); } );

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

    static private Procedure numberQ =
            new TypePredicate( "scream:number?", Number.class );

    static private Procedure numEq =
            new NumberComparisonSupport( "=", (a,b) -> { return a.r7rsEqual( b ); } );

    static private Procedure outputPortQ =
            new TypePredicate( "scream:output-port?", PortOut.class );

    static private Procedure portQ =
            new TypePredicate( "scream:port?", Port.class );

    static private Procedure procedureQ =
            new TypePredicate( "scream:procedure?", Procedure.class );

    static private Procedure realQ =
            new TypePredicate( "scream:real?", Real.class );

    static private Procedure stringQ =
            new TypePredicate( "scream:string?", SchemeString.class );

    static private Procedure symbolQ =
            new TypePredicate( "scream:symbol?", Symbol.class );

    static private Procedure vectorQ =
            new TypePredicate( "scream:vector?", Vector.class );

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
        tle.setPrimitive( binaryInputPortQ );
        tle.setPrimitive( binaryOutputPortQ );
        tle.setPrimitive( booleanQ );
        tle.setPrimitive( bytevectorQ );
        tle.setPrimitive( car );
        tle.setPrimitive( cdr );
        tle.setPrimitive( charQ );
        tle.setPrimitive( consQ );
        tle.setPrimitive( eq );
        tle.setPrimitive( eqv );
        tle.setPrimitive( equal );
        tle.setPrimitive( gtq );
        tle.setPrimitive( gteq );
        tle.setPrimitive( integerQ );
        tle.setPrimitive( inputPortQ );
        tle.setPrimitive( ltq );
        tle.setPrimitive( lteq );
        tle.setPrimitive( nullq );
        tle.setPrimitive( numEq );
        tle.setPrimitive( numberQ );
        tle.setPrimitive( outputPortQ );
        tle.setPrimitive( portQ );
        tle.setPrimitive( procedureQ );
        tle.setPrimitive( realQ );
        tle.setPrimitive( stringQ );
        tle.setPrimitive( symbolQ );
        tle.setPrimitive( vectorQ );

        return tle;
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

    private static class NumberComparisonSupport extends Procedure
    {
        private final BiFunctionX<Number,Number, Boolean,RuntimeX> _f;
        private final Symbol _name;

        public NumberComparisonSupport( String name, BiFunctionX<Number,Number, Boolean,RuntimeX> f )
        {
            super( "scream:" + name, null );

            _f = f;
            _name = Symbol.createObject( name );
        }

        private Thunk _exec( int pos, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            var first =
                    Scut.assertType( Number.class, args.listRef( 0 ), _name, pos );

            if ( args.length() == 1 )
                return c.accept( Bool.T );

            var next =
                    Scut.assertType( Cons.class, args.getCdr(), _name, pos );
            var second =
                    Scut.assertType( Number.class, args.listRef( 1 ), _name, pos+1 );

            if ( ! _f.apply( first, second ) )
                return c.accept( Bool.F );

            return () -> { return _exec(
                    pos+1,
                    next,
                    c );
            };
        }

        @Override
        protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                throws RuntimeX
        {
            checkArgumentCount( 1, Integer.MAX_VALUE, args );

            return _exec( 1, args, c );
        }
    }

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

    private static class TypePredicate extends Procedure
    {
        private final Class<?> _class;

        protected TypePredicate( String name, Class<?> cl )
        {
            super( name, null );

            _class = cl;
        }

        @Override
        protected Thunk _executeImpl(
                Environment e,
                Cons args,
                Cont<FirstClassObject> c )
                        throws RuntimeX
        {
            checkArgumentCount( 1, args );

            var arg = args.getCar();

            if ( arg == Cons.NIL )
                return c.accept( Bool.F );

            return c.accept(
                    _class.isAssignableFrom( arg.getClass() ) ?
                    Bool.T :
                    Bool.F );
        }
    };
}
