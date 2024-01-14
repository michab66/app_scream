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
 * {@code (set! <variable> <expression>)} syntax
 * <p>
 * {@code r7rs 4.1.6 p14}
 */
public final class SyntaxAssign extends Syntax
{
    private SyntaxAssign()
    {
        super( "set!" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, args );

        var symbol = Scut.as(
                Symbol.class,
                args.getCar() );
        var value = args.listRef( 1 );

        return Primitives._eval(
                e,
                value,
                result -> Primitives._assign(
                        e,
                        symbol,
                        result,
                        ignored -> c.accept( Cons.NIL ) ) );
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
        tle.setPrimitive( new SyntaxAssign() );

        return tle;
    }
}
