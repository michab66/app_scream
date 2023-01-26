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
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Scut;

public final class SyntaxLambda extends Syntax
{
    private SyntaxLambda()
    {
        super( "lambda" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Scc<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );
        var formals = args.listRef( 0 );
        checkArgument( 0, formals, Symbol.class, Cons.class );
        var body = Scut.as( Cons.class,args.getCdr() );

        return c.accept(
                new Procedure( e, formals, body ) );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxLambda() );

        return tle;
    }
}
