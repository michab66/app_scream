/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import org.smack.util.TimeProbe;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Scc;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.util.Continuation.Thunk;

/**
 * (%time expression)
 * <p>
 * Returns a pair where the car part holds the time that {@code expression}
 * needed to execute and the cdr holds the result of {@code expression}.
 */
public class SyntaxTime extends Syntax
{
    private SyntaxTime()
    {
        super("%time" );
    }

    /**
     * Processes the result of the timing request.
     *
     * @param tp The timer.
     * @param evalResult The result of expression evaluation.
     * @param c The final continuation receiving the pair.
     * @return The thunk.
     * @throws RuntimeX
     */
    private Thunk finish(
            TimeProbe tp,
            FirstClassObject evalResult,
            Scc<FirstClassObject> c ) throws RuntimeX
    {
        var result = new Cons(
                SchemeInteger.createObject( tp.stop().duration() ),
                evalResult );

        return c.accept( result );
    }

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Scc<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var expression = args.getCar();

        return () -> {
            TimeProbe tp =
                    new TimeProbe( getName().toString() ).start();
            return Primitives._x_eval(
                    e,
                    expression,
                    fco -> finish( tp, fco, c ) );
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
        tle.setPrimitive( new SyntaxTime() );

        return tle;
    }
}
