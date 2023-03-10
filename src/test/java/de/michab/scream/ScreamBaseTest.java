/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.function.Function;

import org.smack.util.JavaUtil;

import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.SchemeParser;

public class ScreamBaseTest
{
    public final static SchemeInteger i1 = i( 1 );
    public final static SchemeInteger i2 = i( 2 );
    public final static SchemeInteger i3 = i( 3 );
    public final static SchemeInteger i4 = i( 4 );
    public final static SchemeInteger i313 = i( 313 );

    public final static Symbol s1 = s( "one" );
    public final static Symbol s2 = s( "two" );
    public final static Symbol s3 = s( "three" );
    public final static Symbol s4 = s( "four" );
    public final static Symbol s313 = s( "threethirteen" );

    public static <S extends FirstClassObject,J>
    void toJava_( Class<S> sc, Class<J> jc, J testObject, Function<J, S> factory) throws RuntimeX
    {
        S so = factory.apply( testObject );
        assertNotNull( so );
        assertInstanceOf( sc, so );
        var j = so.toJava();
        assertNotNull( j );
        assertInstanceOf( jc, j );
        assertEquals( testObject, j );
    }

    static public <S extends FirstClassObject> S readSingleExpression(
            String scheme, Class<S> cl )
                    throws RuntimeX
    {
        var parser =
                new SchemeParser( scheme );
        var fco =
                parser.getExpression();
        assertEquals(
                Port.EOF,
                parser.getExpression() );
        assertInstanceOf( cl, fco );
        return cl.cast( fco );
    }

    static public FirstClassObject parse(
            String scheme )
                    throws RuntimeX
    {
        var parser =
                new SchemeParser( scheme );
        var fco =
                parser.getExpression();
        assertEquals(
                Port.EOF,
                parser.getExpression() );
        return fco;
    }

    private FirstClassObject _contTestImpl(
            String expression,
            FirstClassObject expected,
            Code expectedError
            ) throws RuntimeX
    {
        JavaUtil.Assert( expected != null || expectedError != null );

        ScreamEvaluator se = (ScreamEvaluator)new Scream().getScriptEngine();

        FirstClassObject opCall =
                new SchemeParser( expression ).getExpression();

        assertInstanceOf( Cons.class, opCall );

        var env = se.getInteraction();

        FirstClassObject result =
                null;
        RuntimeX error =
                null;

        try {
            result = Scream.toStack(
                c -> opCall.evaluate( env, c) );
        }
        catch ( RuntimeX e )
        {
            error = e;
        }

        if ( expectedError != null )
        {
            assertNotNull( error );
            assertEquals( expectedError, error.getCode() );
            assertNull( result );
            return Cons.NIL;
        }

        if ( error != null )
        {
            fail( error.getMessage() );
        }

        return result;
    }

    protected void expectFco(
            String expression,
            FirstClassObject expected )
                    throws RuntimeX
    {
        var fco = _contTestImpl( expression, expected, null );

        assertNotNull(
                fco );
        assertInstanceOf(
                expected.getClass(), fco );
        assertEqualq(
                expected,
                fco );
    }

    protected void _contTest(
            String expression,
            FirstClassObject expected,
            Function<FirstClassObject, Boolean> eq
            )
                    throws RuntimeX
    {
        var fco = _contTestImpl( expression, expected, null );

        assertNotNull(
                fco );
        assertInstanceOf(
                expected.getClass(), fco );
        assertTrue(
                eq.apply( fco ) );
    }

    /**
     * Evaluate the passed expression and expect an error.
     *
     * @param expression The expression to evaluate.
     * @param expected The expected error.
     * @throws RuntimeX
     */
    protected void expectError(
            String expression,
            Code expected )
                    throws RuntimeX
    {
        _contTestImpl( expression, null, expected );
    }

    protected void assertEqualq( FirstClassObject expected, FirstClassObject actual )
    {
        if ( FirstClassObject.equal( expected, actual ) )
            return;

        var msg = String.format(
                "Expected '%s', got '%s'.",
                FirstClassObject.toString( expected ),
                FirstClassObject.toString( actual ) );
        fail( msg );
    }

    /**
     * @return A newly created script engine.
     */
    protected ScreamEvaluator scriptEngine()
    {
        return (ScreamEvaluator)new Scream().getScriptEngine();
    }

    public static Symbol s( String name )
    {
        return Symbol.createObject( name );
    }

    public static SchemeInteger i( long v )
    {
        return SchemeInteger.createObject( v );
    }
    protected static Cons c( FirstClassObject ... fcos )
    {
        return Cons.create( fcos );
    }
}
