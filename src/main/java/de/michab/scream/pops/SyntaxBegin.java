package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.Operation;
import de.michab.scream.RuntimeX;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;

/**
 * (begin exp1 exp2 ...) library syntax; r7rs 17
 */
public class SyntaxBegin extends Operation
{
    private SyntaxBegin()
    {
        super( "begin" );

    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        L l = (e,c) -> Continuation._x_begin(
                e,
                args,
                c);

        return new Lambda( l, getName() );
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

    @Override
    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        return Continuation._x_begin( e, args, c );
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
