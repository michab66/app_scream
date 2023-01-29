/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Scc;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Scut;
import de.michab.scream.util.Continuation.Thunk;

/**
 * {@code (%syntax <signature>) exp1 ... )}
 * <br>
 * {@code (%syntax (xquote value) value)} <br>
 * {@code (%syntax (xquote . rest) rest)}
 * <p>
 * Similar to 'define'.  Generates macros whose body gets called
 * with unevaluated arguments.
 */
public class SyntaxSyntax extends Syntax
{
    private SyntaxSyntax()
    {
        super( "%syntax" );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Scc<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );

        Cons signature = Scut.as(
                Cons.class,
                args.listRef( 0 ),
                s-> {
                    throw new RuntimeX( Code.SYNTAX_ERROR );
                } );
        Symbol name = Scut.as(
                Symbol.class,
                signature.listRef( 0 ),
                s -> {
                    throw new RuntimeX( Code.DEFINE_ERROR );
                } );
        FirstClassObject parameterList =
                signature.getCdr();
        Cons body = Scut.as(
                Cons.class,
                args.listTail( 1 ) );

        return () -> {
            var value = new Syntax(
                    e,
                    parameterList,
                    body ).setName( name );
            return Primitives._x_define( e, name, value, c );
        };
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxSyntax() );

        return tle;
    }
}
