/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.Syntax;

/**
 * Switch off evaluation for the single passed argument.
 *
 * (quote <datum>) syntax; r5rs 8
 */
public class SyntaxQuote extends Syntax
{
    private SyntaxQuote()
    {
        super( "quote" );
    }

    @Override
    protected Thunk _execute( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var quoted = args.getCar();

        return Primitives._x_quote(
                        e,
                        quoted,
                        c );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxQuote() );

        return tle;
    }
}
