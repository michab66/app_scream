package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.function.Function;

import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

public class TestUtil
{
    public final static SchemeInteger i1 = SchemeInteger.createObject( 1 );
    public final static SchemeInteger i2 = SchemeInteger.createObject( 2 );
    public final static SchemeInteger i3 = SchemeInteger.createObject( 3 );
    public final static SchemeInteger i4 = SchemeInteger.createObject( 4 );
    public final static SchemeInteger i313 = SchemeInteger.createObject( 313 );

    public final static Symbol s1 = Symbol.createObject( "one" );
    public final static Symbol s2 = Symbol.createObject( "two" );
    public final static Symbol s3 = Symbol.createObject( "three" );
    public final static Symbol s4 = Symbol.createObject( "four" );
    public final static Symbol s313 = Symbol.createObject( "threethirteen" );

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
}
