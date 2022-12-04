/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.Lambda;
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
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var quoted = args.getCar();

        return new Lambda(
                (e,c) -> Primitives._x_quote(
                        e,
                        quoted,
                        c ),
                this.toString() );
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
