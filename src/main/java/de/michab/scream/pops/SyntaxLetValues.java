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
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;
import de.michab.scream.util.Scut.ConsumerX;

/**
 * {code (let-values <mv binding spec> <body>)} syntax<br>
 * {code (let*-values <mv binding spec> <body>)} syntax<br>
 * <p>
 * {code r7rs 4.2.2 17}
 */
public abstract class SyntaxLetValues
    extends Syntax
{
    private SyntaxLetValues( String name )
    {
        super( name );
    }

    /**
     * A proper list with length greater than 0 of two element lists, each
     * having a symbol as the first element.
     *
     * @return A proper list of symbols that are assigned in the binding list.
     */
    protected void validateBindings( Cons bindings ) throws RuntimeX
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
            // First element is a list.
            var symbolList = Scut.as(
                    Cons.class,
                    binding.getCar(),
                    s -> {
                        throw RuntimeX.mBadBinding( getName(), binding );
                    } );
            // Must be proper.
            if ( ! Cons.isProper( symbolList ) )
                throw RuntimeX.mExpectedProperList();
            Scut.assertUnique( symbolList );
            Scut.assertHomogeneous( symbolList, Symbol.class );
        }
    }

    /**
     * {code (let-values <mv binding spec> <body>) syntax}
     */
    static public final Syntax letValuesSyntax = new SyntaxLetValues( "let-values" )
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

            return Primitives._x_let_values(
                    e,
                    e.extend( getName() ),
                    bindings,
                    body,
                    c);
        }
    };

    /**
     * {code (let*-values <mv binding spec> <body>) syntax}
     */
    static public final Syntax letAsteriskValuesSyntax = new SyntaxLetValues( "let*-values" )
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

            return Primitives._x_let_values(
                    extended,
                    extended,
                    bindings,
                    body,
                    c);
        }
    };
}
