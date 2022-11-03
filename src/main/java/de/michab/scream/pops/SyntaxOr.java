package de.michab.scream.pops;

import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.Syntax;

/**
 * (or <test1> ... ) syntax; r5rs 11
 *
 * The test expressions are evaluated from left to right, and the value of
 * the first expression that evaluates to a true value (see section 6.3.1) is
 * returned. Any remaining expressions are not evaluated. If all expressions
 * evaluate to false values, the value of the last expression is returned. If
 * there are no expressions then #f is returned.
 */
public class SyntaxOr extends Syntax
{
    SyntaxOr()
    {
        super( "or" );
    }

    @Override
    public FirstClassObject compile( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        // Constant expression optimisation.
        if ( args.length == 0 )
            return SchemeBoolean.F;

        // Compile all passed expressions.
        for ( int i = args.length-1 ; i >= 0 ; i-- )
            args[i] = compile( args[i], parent );

        return new ShortcutOr( args );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxOr() );

        return tle;
    }
}
