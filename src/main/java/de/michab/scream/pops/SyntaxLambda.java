package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Operation;
import de.michab.scream.Procedure;
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.util.Scut;

public final class SyntaxLambda extends Operation
{
    private SyntaxLambda()
    {
        super( "lambda" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 2, Integer.MAX_VALUE, args );
        var formals = args.listRef( 0 );
        checkArgument( 0, formals, Symbol.class, Cons.class );
        var body = Scut.as( Cons.class,args.getCdr() );

        Lambda.L result = (e,c) -> {
            return  c.accept(
                    new Procedure( env, formals, body ) );
        };

        return new Lambda( result, getName() ).setInfo( args );
    }

    /**
     *
     */
    @Override
    public FirstClassObject compile( Environment parent, Cons args )
            throws RuntimeX
    {
        return _compile( parent, args );
    }
    @Override
    public FirstClassObject activate( Environment parent,
            Cons arguments )
                    throws RuntimeX
    {
        var λ = _compile( parent, arguments );

        return FirstClassObject.evaluate( λ, parent );
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
