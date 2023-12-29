/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code (%syntax <signature>) exp1 ... )}<br>
 * {@code (%syntax (xquote value) value)} <br>
 * {@code (%syntax (xquote . rest) rest)}
 * <p>
 * Similar to 'define'.  Generates macros whose body gets called
 * with unevaluated arguments.
 */
public class SyntaxSyntax extends Syntax
{
    private SyntaxSyntax()
    {
        super( "%syntax" );
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        Cons signature = Scut.as(
                Cons.class,
                args.listRef( 0 ),
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
            var value = new Syntax(
                    parameterList,
                    body ).setName( name );
            return Primitives._define(
                    e,
                    name,
                    value,
                    ignore -> Primitives._quote( Cons.NIL, c ) );
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
        tle.setPrimitive( new SyntaxSyntax() );

        return tle;
    }
}
