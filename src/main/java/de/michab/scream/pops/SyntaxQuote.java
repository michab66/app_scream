/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Scc;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;

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
    protected Thunk _executeImpl( Environment e, Cons args,
            Scc<FirstClassObject> c ) throws RuntimeX
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
