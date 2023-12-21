/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;
import de.michab.scream.util.Scut.ConsumerX;

/**
 * {code (let <bindings> <body>)} syntax<br>
 * {code (let* <bindings> <body>)} syntax<br>
 * {code (letrec <bindings> <body>)} syntax<br>
 * <p>
 * {code r7rs 4.2.2 p16}
 */
public abstract class SyntaxLet
    extends Syntax
{
    private SyntaxLet( String name )
    {
        super( name );
    }

    /**
     * A proper list with length greater than 0 of two element lists, each having
     * a symbol as the first element.
     *
     * @return A proper list of symbols that are assigned in the binding list.
     */
    protected Cons validateBindings( Cons bindings ) throws RuntimeX
    {
        final var originalBindings = bindings;
        ConsumerX<Long> ic = s -> {
            throw RuntimeX.mBadBinding( getName(), originalBindings );
        };

        Scut.checkProperLength(
                bindings,
                1,
                Integer.MAX_VALUE,
                ic ,
                ic );

        Cons result = Cons.NIL;

        for ( var c : bindings )
        {
            // Car is a list.
            var binding = Scut.as(
                    Cons.class,
                    c,
                    s-> {
                        throw RuntimeX.mBadBinding( getName(), originalBindings );
                    });
            // Of length 2.
            Scut.checkProperLength(
                    binding,
                    2,
                    2,
                    ic,
                    ic );
            // First element is symbol.
            var symbol = Scut.as(
                    Symbol.class,
                    binding.getCar(),
                    s -> {
                        throw RuntimeX.mBadBinding( getName(), binding );
                    } );

            result = new Cons( symbol, result );
        }

        return result;
    }

    /**
     * {@code (let <bindings> <body>) syntax r7rs, p16}
     * <p>
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static public final Syntax letSyntax = new SyntaxLet( "let" )
    {
        private Thunk namedLet(
                Environment e,
                Cons args,
                Cont<FirstClassObject> c )
            throws RuntimeX
        {
            checkArgumentCount( 3, Integer.MAX_VALUE, args );

            Symbol variable =
                    Scut.as( Symbol.class, args.listRef( 0 ) );
            Cons bindings =
                    Scut.as( Cons.class, args.listRef( 1 ) );
            var body =
                    Scut.as( Cons.class, args.listTail( 2 ) );

            var arguments = Scut.assertUnique(
                    validateBindings( bindings ) ).reverse();

            // TODO this is a testcase for #151
            var array = bindings.asArray();
            for ( int i = 0 ; i < array.length ; i++ )
            {
                array[i] = Scut.as( Cons.class, array[i] ).getCdr();
                array[i] = Scut.as( Cons.class, array[i] ).getCar();
            }
            var ebindings = Cons.create( array );


            var extended = e.extend( "named-let" );

            var p = new Procedure(
                    extended,
                    arguments,
                    body );
            p.setName( variable );

            extended.define( variable, p );

            Cont<Cons> exec = evaluatedArguments -> {
                return p.apply(
                        extended,
                        evaluatedArguments,
                        c );
            };

            // Evaluate the arguments in the received environment.
            return () -> Primitives._x_evalCons(
                    e,
                    ebindings,
                    exec );
        }

        @Override
        protected Thunk _executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            if ( FirstClassObject.is( Symbol.class, args.getCar() ) )
                return namedLet( e, args, c );

            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            Cons bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            Scut.assertUnique(
                    validateBindings( bindings ) );

            return Primitives._x_let(
                    e,
                    e.extend( getName() ),
                    bindings,
                    body,
                    c);
        }
    };

    /**
     * {@code (let* <bindings> <body>) syntax r7rs, p16}
     * <p>
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static public final Syntax letAsteriskSyntax = new SyntaxLet( "let*" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            validateBindings( bindings );

            var extended = e.extend( getName() );

            return Primitives._x_let(
                    extended,
                    extended,
                    bindings,
                    body,
                    c);
        }
    };

    /**
     * {code (letrec <bindings> <body>)} syntax<br>
     * <p>
     * {code r7rs 4.2.2 p16}
     */
    static public final Syntax letrecSyntax = new SyntaxLet( "letrec" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            var symbols = validateBindings( bindings );

            return Primitives._x_letRec(
                    e,
                    bindings,
                    body,
                    symbols,
                    c);
        }
    };
}
