package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
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
                (e,c) -> Continuation._x_quote(
                        e,
                        quoted,
                        c ),
                this.toString() );
    }

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
        tle.setPrimitive( new SyntaxQuote() );

        return tle;
    }
}
