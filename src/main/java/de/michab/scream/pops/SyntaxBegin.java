/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.RuntimeX;
import de.michab.scream.Syntax;

/**
 * (begin exp1 exp2 ...) library syntax; r7rs 17
 */
public class SyntaxBegin extends Syntax
{
    private SyntaxBegin()
    {
        super( "begin" );

    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        L l = (e,c) -> Primitives._x_begin(
                e,
                args,
                c);

        return new Lambda( l, getName() );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxBegin() );

        return tle;
    }
}
