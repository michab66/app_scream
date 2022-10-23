/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.function.Function;

import org.smack.util.JavaUtil;

import de.michab.scream.ScreamException.Code;
import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;
import urschleim.Continuation;
import urschleim.Holder;

public class TestUtil
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

    static <S extends FirstClassObject,J>
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
                    throws FrontendX
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

    private void _contTest(
            String expression,
            FirstClassObject expected,
            Code expectedError
            ) throws RuntimeX
    {
        JavaUtil.Assert( expected != null || expectedError != null );

        SchemeEvaluator2 se = (SchemeEvaluator2)new SchemeInterpreter2().getScriptEngine();

        FirstClassObject opCall =
                new SchemeParser( expression ).getExpression();

        assertInstanceOf( Cons.class, opCall );

        var env = se.getInteraction();

        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                opCall.evaluate( env,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( expectedError != null )
        {
            assertNotNull( error.get() );
            assertEquals( expectedError, error.get().getCode() );
            assertNull( r.get() );
            return;
        }

        if ( error.get() != null )
        {
            fail( error.get().getMessage() );
        }

        assertNotNull(
                r.get() );
        assertInstanceOf(
                expected.getClass(), r.get() );
        assertEquals(
                expected,
                r.get() );
    }

    protected void _contTest(
            String expression,
            FirstClassObject expected )
                    throws RuntimeX
    {
        _contTest( expression, expected, null );
    }

    protected void _contTest(
            String expression,
            Code expected )
                    throws RuntimeX
    {
        _contTest( expression, null, expected );
    }

    public static Symbol s( String name )
    {
        return Symbol.createObject( name );
    }

    public static SchemeInteger i( long v )
    {
        return SchemeInteger.createObject( v );
    }
}
