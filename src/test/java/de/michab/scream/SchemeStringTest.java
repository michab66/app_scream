package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

public class SchemeStringTest
{
    @Test
    public void toJava() throws Exception
    {
        TestUtil.toJava_(
                SchemeString.class,
                String.class,
                "micbinz",
                SchemeString::new );
    }

    @Test
    public void constructorLength() throws Exception
    {
        final var L = 5;
        final var F = SchemeCharacter.SPACE.asCharacter();

        var s = new SchemeString( L );
        assertEquals( L, s.length() );
        assertFalse( s.isConstant() );

        for ( var i = 0 ; i < s.length() ; i++ )
        {
            assertEquals(
                    F,
                    s.getCharAt( i ) );
        }

        try
        {
            s.getCharAt( L );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( ScreamException.Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }

        var j = s.toJava();
        assertNotNull( j );
        assertInstanceOf( String.class, j );
        String js = (String)j;
        assertEquals( L, js.length() );
        for ( var i = 0 ; i < js.length() ; i++ )
        {
            assertEquals(
                    F,
                    js.charAt( i ) );
        }
    }

    @Test
    public void constructorLengthFiller() throws Exception
    {
        final var L = 5;
        final var F = '!';

        var s = new SchemeString( L, F );
        assertEquals( L, s.length() );

        for ( var i = 0 ; i < s.length() ; i++ )
        {
            assertEquals(
                    F,
                    s.getCharAt( i ) );
        }

        var j = s.toJava();
        assertNotNull( j );
        assertInstanceOf( String.class, j );
        String js = (String)j;
        assertEquals( L, js.length() );
        for ( var i = 0 ; i < js.length() ; i++ )
        {
            assertEquals(
                    F,
                    js.charAt( i ) );
        }
    }

    @Test
    public void constructorFromString() throws Exception
    {
        {
            var S = "313";
            var s = new SchemeString( S );
            assertEquals( S.length(), s.length() );
            assertEquals( S, s.toJava() );
            assertTrue( s.isConstant() );

            try
            {
                s.setCharAt( 0, 'x' );
                fail();
            }
            catch ( ScreamException e )
            {
                assertEquals( ScreamException.Code.CANT_MODIFY_CONSTANT, e.getCode() );
            }
        }
        {
            var S = "313";
            var s = new SchemeString( S, false );
            assertEquals( S.length(), s.length() );
            assertEquals( S, s.toJava() );
            assertFalse( s.isConstant() );
            s.setCharAt( 0, 'x' );
            assertEquals( "x13", s.toJava() );
            s.fill( '#' );
            assertEquals( "###", s.toJava() );
        }
    }

    @Test
    public void compareTo() throws Exception
    {

        var thirteen = new SchemeString( "313" );
        var fourteen = new SchemeString( "314" );
        var zero = SchemeDouble.createObject( 0.0 );

        assertEquals( -1, thirteen.compareTo( fourteen ) );
        assertEquals( 0, fourteen.compareTo( fourteen ) );
        assertEquals( 1, fourteen.compareTo( thirteen ) );
    }

    @Test
    public void equal() throws Exception
    {

        var thirteen = new SchemeString( "313" );
        var fourteen = new SchemeString( "314" );
        var zero = SchemeDouble.createObject( 0.0 );

        assertTrue( thirteen.equal( thirteen ) );
        assertFalse( fourteen.equal( thirteen ) );
        assertFalse( fourteen.equal( zero ) );
        assertFalse( fourteen.equal( Cons.NIL ) );
    }

    @Test
    public void cloneTest() throws Exception
    {

        var thirteen = new SchemeString( "313" );
        SchemeString clone = (SchemeString)thirteen.clone();

        assertTrue( thirteen.isConstant() );
        assertFalse( clone.isConstant() );
        assertTrue( clone.equal( thirteen ) );
    }
}
