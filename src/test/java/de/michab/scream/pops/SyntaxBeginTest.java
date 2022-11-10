package de.michab.scream.pops;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.Scream;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.ScreamException;
import de.michab.scream.frontend.SchemeParser;
import urschleim.Holder;

public class SyntaxBeginTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                begin
                """ );
        assertInstanceOf( Operation.class, result );
    }

    @Test
    void _begin() throws Exception
    {
        Scream si = new Scream();
        ScreamEvaluator se = (ScreamEvaluator)si.getScriptEngine();

        Environment env =
                se.getInteraction();

        Cons cons = (Cons)new SchemeParser(
            """
                ((define one 1)
                 (define two 2)
                 313)
            """ ).getExpression();

        Holder<FirstClassObject> r =
                new Holder<>( null );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                SyntaxBegin._begin(
                        env,
                        cons,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        assertEqualq( i313, r.get() );
        assertEqualq( i1, env.get( s1 ) );
        assertEqualq( i2, env.get( s2 ) );
    }
}
