package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class VectorTest
{
    @Test
    public void basic() throws Exception
    {
        var size = 5;
        Vector v = new Vector( size );

        assertEquals( size, v.getSize() );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( Cons.NIL, v.getElement( i ) );

        v.fill( TestUtil.s1 );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( TestUtil.s1, v.getElement( i ) );
    }
}
