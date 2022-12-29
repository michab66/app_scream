/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class SchemeCharacterTest
{
    private final Character bang = '!';

    @Test
    public void toJava() throws Exception
    {
        ScreamBaseTest.toJava_(
                SchemeCharacter.class,
                Character.class,
                bang,
                SchemeCharacter::createObject );
    }

    @Test
    public void identity() throws Exception
    {
        SchemeCharacter c = SchemeCharacter.createObject( bang );
        assertNotNull( c );
        assertTrue( c ==  SchemeCharacter.createObject( bang ) );
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
