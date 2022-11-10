package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.Symbol;
import de.michab.scream.util.Scut;

/**
 * (set! <variable> <expression>) syntax; r7rs 14
 */
public final class SyntaxAssign extends Operation
{
    private SyntaxAssign()
    {
        super( "set!" );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 2, args );

        var symbol = Scut.as(
                Symbol.class,
                args.getCar() );
        var value = args.listRef( 1 );

        L l = (e,c) -> Continuation._assign(
                e,
                symbol,
                value,
                c);

        return new Lambda( l, getName() );
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
        tle.setPrimitive( new SyntaxAssign() );

        return tle;
    }
}
