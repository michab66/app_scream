package de.michab.scream.pops;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeInteger;
import de.michab.scream.Syntax;

/**
 * (%time exp)
 *
 * Returns a pair, where the car part holds the time that <code>exp</code>
 * needed to execute and the cdr holds the result of <code>exp</code>.
 */
public class SyntaxTime extends Syntax
{
    private SyntaxTime()
    {
        super("%time" );
    }

    @Override
    public FirstClassObject activate( Environment parent, FirstClassObject[] args )
            throws RuntimeX
    {
        checkArgumentCount( 1, args );

        // Trigger an explicit garbage collection before starting with the time
        // measurements.
        System.gc();

        // Get the start time...
        long startTime = System.currentTimeMillis();
        // ...do the actual evaluation...
        FirstClassObject resultCdr = evaluate( args[0], parent );
        // ...and compute the time that was needed for the evaluation.
        FirstClassObject resultCar = SchemeInteger.createObject(
                System.currentTimeMillis() - startTime );

        return new Cons( resultCar, resultCdr );
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the top level environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( new SyntaxTime() );

        return tle;
    }
}
