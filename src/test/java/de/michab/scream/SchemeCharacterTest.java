package de.michab.scream;

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
        var j = c.convertToJava();
        assertNotNull( j );
        assertInstanceOf( Character.class, j );

        var c2 = SchemeCharacter.createObject( ((Character)j).charValue() );

        assertTrue( c == c2 );
    }
}
