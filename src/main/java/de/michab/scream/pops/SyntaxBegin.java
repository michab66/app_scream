/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.Continuation.Thunk;
import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Scc;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Syntax;

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
    protected Thunk _executeImpl( Environment e, Cons args,
            Scc<FirstClassObject> c ) throws RuntimeX
    {
        return Primitives._x_begin( e, args, c );
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
