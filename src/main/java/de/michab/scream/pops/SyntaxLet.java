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
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.util.Scut;
import de.michab.scream.util.Scut.ConsumerX;

/**
 *
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
            throw Scut.mBadBinding( getName(), originalBindings );
        };

        Scut.checkProperLength(
                bindings,
                1,
                Integer.MAX_VALUE,
                ic ,
                ic );

        Cons result = Cons.NIL;

        while ( bindings != Cons.NIL )
        {
            // Car is a list.
            var c = Scut.as(
                    Cons.class,
                    bindings.getCar(),
                    s-> {
                        throw Scut.mBadBinding( getName(), originalBindings );
                    });
            // Of length 2.
            Scut.checkProperLength(
                    c,
                    2,
                    2,
                    ic,
                    ic );
            // First element is symbol.
            var symbol = Scut.as(
                    Symbol.class,
                    c.getCar(),
                    s -> {
                        throw Scut.mBadBinding( getName(), c );
                    } );

            result = new Cons( symbol, result );

            bindings = (Cons)bindings.getCdr();
        }

        return result;
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
     * (let <bindings> <body>) syntax r7rs, p16
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letSyntax = new SyntaxLet( "let" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            validateBindings( bindings );

            L l = (e,c) -> Continuation._x_let(
                    e,
                    bindings,
                    body,
                    c);

            return new Lambda( l, getName() );
        }
    };

    /**
     * (let* <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letAsteriskSyntax = new SyntaxLet( "let*" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            validateBindings( bindings );

            L l = (e,c) -> Continuation._x_letStar(
                    e,
                    bindings,
                    body,
                    c);

            return new Lambda( l, getName() );
        }
    };

    /**
     * (letrec <bindings> <body>) syntax r5rs, 11
     * where bindings is ((variable1 init1) ...) and body is a sequence of
     * expressions.
     */
    static private Syntax letrecSyntax = new SyntaxLet( "letrec" )
    {
        @Override
        protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
        {
            checkArgumentCount( 2, Integer.MAX_VALUE, args );

            var bindings =
                    Scut.as( Cons.class, args.getCar() );
            var body =
                    Scut.as( Cons.class, args.getCdr() );

            var symbols = validateBindings( bindings );

            L l = (e,c) -> Continuation._x_letRec(
                    e,
                    bindings,
                    body,
                    symbols,
                    c);

            return new Lambda( l, getName() );
        }
    };

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( letAsteriskSyntax );
        tle.setPrimitive( letSyntax );
        tle.setPrimitive( letrecSyntax );

        return tle;
    }
}
