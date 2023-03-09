/*
 * Scream @ https://github.com/michab/dev_smack
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

/**
 * {code (define <variable> <expression>)} syntax<br>
 * {code (define (<variable> <formals>) <body>)} syntax<br>
 * {code (define (<variable> . <formal>) <body>)} syntax<br>
 * <p>
 * {@code r7rs 5.3 p25 }
 */
public class SyntaxDefine extends Syntax
{
    private SyntaxDefine()
    {
        super( "define" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        var variableSlot = args.getCar();

        var rest = Scut.as(
                Cons.class,
                args.getCdr(),
                s-> {
                    throw RuntimeX.mSyntaxError();
                } );

        if ( variableSlot instanceof Symbol ) {
            checkArgumentCount( 1, rest );
            return Primitives._x_eval(
                    e,
                    rest.getCar(),
                    fco -> Primitives._x_define(
                            e,
                            (Symbol)variableSlot,
                            fco,
                            c ) );

        }

        // This is a lambda define...

        Cons signature = Scut.as(
                Cons.class,
                variableSlot,
                s-> {
                    throw RuntimeX.mSyntaxError();
                } );
        Symbol name = Scut.as(
                Symbol.class,
                signature.listRef( 0 ),
                s -> {
                    throw RuntimeX.mDefineError();
                } );
        FirstClassObject parameterList =
                signature.getCdr();
        Cons body = Scut.as(
                Cons.class,
                args.listTail( 1 ) );

        return () -> {
            var value = new Procedure(
                    e,
                    parameterList,
                    body ).setName( name );
            return Primitives._x_define( e, name, value, c );
        };
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
        tle.setPrimitive( new SyntaxDefine() );

        return tle;
    }
}
