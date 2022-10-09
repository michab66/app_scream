package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class SchemeCharacterTest
{
    @Test
    public void basic() throws Exception
    {
        SchemeCharacter c = SchemeCharacter.createObject( '!' );
        assertNotNull( c );
        var j = c.toJava();
        assertNotNull( j );
        assertInstanceOf( Character.class, j );

        var c2 = SchemeCharacter.createObject( ((Character)j).charValue() );

        assertTrue( c == c2 );
    }

    @Test
    public void newline() throws Exception
    {
        SchemeCharacter c = SchemeCharacter.createObject( '\n' );
        assertNotNull( c );
        assertTrue( c == SchemeCharacter.NEWLINE );
        assertEquals( "#\\newline", c.toString() );
    }

    @Test
    public void space() throws Exception
    {
        SchemeCharacter c = SchemeCharacter.createObject( ' ' );
        assertNotNull( c );
        assertTrue( c == SchemeCharacter.SPACE );
        assertEquals( "#\\space", c.toString() );
    }

    @Test
    public void ascii() throws Exception
    {
        SchemeCharacter c = SchemeCharacter.createObject( '8' );
        assertNotNull( c );
        assertEquals( "#\\8", c.toString() );
    }
}
