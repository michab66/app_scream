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
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code (lambda <formals> <body>)} syntax
 * <p>
 * {@code r7rs 4.1.4 p13}
 */
public final class SyntaxLambda extends Syntax
{
    private SyntaxLambda()
    {
        super( "lambda" );
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );
        var formals = args.listRef( 0 );
        checkFormals( formals );
        var body = Scut.as( Cons.class,args.getCdr() );

        return c.accept(
                new Procedure( e, formals, body ) );
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
        tle.setPrimitive( new SyntaxLambda() );

        return tle;
    }
}
