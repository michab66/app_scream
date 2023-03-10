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
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;

/**
 * {@code (quote <datum>)} syntax
 * <p>
 * {@code r7rs 4.1.2 p12}
 */
public class SyntaxQuote extends Syntax
{
    private SyntaxQuote()
    {
        super( "quote" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var quoted = args.getCar();

        return Primitives._x_quote(
                        e,
                        FirstClassObject.setConstant( quoted ),
                        c );
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
        tle.setPrimitive( new SyntaxQuote() );

        return tle;
    }
}
