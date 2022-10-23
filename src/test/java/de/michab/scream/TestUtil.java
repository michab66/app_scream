package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.function.Function;

import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

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

    public static Symbol s( String name )
    {
        return Symbol.createObject( name );
    }

    public static SchemeInteger i( long v )
    {
        return SchemeInteger.createObject( v );
    }
}
