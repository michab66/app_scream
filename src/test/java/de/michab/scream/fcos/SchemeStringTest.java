/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class SchemeStringTest
{
    @Test
    public void basic() throws Exception
    {
        assertEquals( 3, new StringBuilder( "313" ).length() );
    }

    @Test
    public void basic_2() throws Exception
    {
        String s = "λ Hélô 丧";
        assertEquals( 8, s.length() );
        byte[] bytes = s.getBytes( StandardCharsets.UTF_8 );
        assertTrue( 8 < bytes.length );
        String s2 = new String( bytes, StandardCharsets.UTF_8 );
        assertEquals( s, s2 );
    }

    @Test
    public void copyTest() throws Exception
    {
        SchemeString s = FirstClassObject.setConstant(
                SchemeString.make( "Motörhead" ) );
        assertTrue( s.isConstant() );
        assertFalse( s.copy().isConstant() );
    }

    @Test
    public void constantness2() throws Exception
    {
        SchemeString s = FirstClassObject.setConstant(
                SchemeString.make( "Motörhead" ) );
        try
        {
            s.fill( 'x' );
            fail();
        }
        catch ( RuntimeX e )
        {
            assertEquals( Code.CANNOT_MODIFY_CONSTANT, e.getCode() );
        }
    }
    @Test
    public void constantness3() throws Exception
    {
        SchemeString s = FirstClassObject.setConstant(
                SchemeString.make( "Motörhead" ) );
        try
        {
            s.setCharAt( 0, 'x' );
            fail();
        }
        catch ( RuntimeX e )
        {
            assertEquals( Code.CANNOT_MODIFY_CONSTANT, e.getCode() );
        }
    }

    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                SchemeString.class,
                String.class,
                "micbinz",
                SchemeString::makeEscaped );
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
        var S = "313";
        var s = SchemeString.make( S );
        assertEquals( S.length(), s.length() );
        assertEquals( S, s.toJava() );
        assertFalse( s.isConstant() );
        s.setCharAt( 0, 'x' );
        assertEquals( "x13", s.toJava() );
        s.fill( '#' );
        assertEquals( "###", s.toJava() );
    }

    @Test
    public void compareTo() throws Exception
    {

        var thirteen = SchemeString.make( "313" );
        var fourteen = SchemeString.make( "314" );

        assertEquals( -1, thirteen.compareTo( fourteen ) );
        assertEquals( 0, fourteen.compareTo( fourteen ) );
        assertEquals( 1, fourteen.compareTo( thirteen ) );
    }

    @Test
    public void equal() throws Exception
    {

        var thirteen = SchemeString.make( "313" );
        var fourteen = SchemeString.make( "314" );
        var zero = SchemeDouble.createObject( 0.0 );

        assertTrue( thirteen.equal( thirteen ) );
        assertFalse( fourteen.equal( thirteen ) );
        assertFalse( fourteen.equal( zero ) );
        assertFalse( fourteen.equal( Cons.NIL ) );
    }

    @Test
    public void appendTest() throws Exception
    {
        var thirteen = SchemeString.make( "313" );
        var thirteenNull = thirteen.append( null );

        assertTrue( thirteen.equals( thirteenNull ) );
    }
}
