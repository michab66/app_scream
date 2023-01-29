/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
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
 * (and <test1> ...) syntax; r7rs p15
 *
 * The test expressions are evaluated from left to right, and the value of
 * the first expression that evaluates to a false value (see section 6.3.1)
 * is returned. Any remaining expressions are not evaluated. If all the
 * expressions evaluate to true values, the value of the last expression is
 * returned.  If there are no expressions then #t is returned.
 */
public class SyntaxAnd extends Syntax
{
    private SyntaxAnd()
    {
        super( "and" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 0, Integer.MAX_VALUE, args );

        return Primitives._x_and(
                e,
                args,
                c);
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxAnd() );

        return tle;
    }
}
