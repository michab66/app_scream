/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.pops;

import org.smack.util.TimeProbe;

import de.michab.scream.Cons;
import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Lambda;
import de.michab.scream.Lambda.L;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeInteger;
import de.michab.scream.Syntax;

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
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var result = new Cons(
                SchemeInteger.createObject( tp.stop().duration() ),
                evalResult );

        return c.accept( result );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        var expression = args.getCar();

        L l = (e,c) -> {
            TimeProbe tp =
                    new TimeProbe( getName().toString() ).start();
            return Primitives._x_eval(
                    e,
                    expression,
                    fco -> finish( tp, fco, c ) );
        };

        return new Lambda( l, getName() );
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
