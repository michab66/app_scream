package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class VectorTest
{
    @Test
    public void basic() throws Exception
    {
        var size = 5;
        Vector v = new Vector( size );

        assertEquals( size, v.size() );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( Cons.NIL, v.get( i ) );

        v.fill( TestUtil.s1 );
        for ( int i = 0 ; i < size ; i++ )
            assertEquals( TestUtil.s1, v.get( i ) );
    }

    @Test
    public void indexErr() throws Exception
    {
        var size = 5;
        Vector v = new Vector( size );

        try {
            v.get( size );
            fail();
        }
        catch ( ScreamException e ) {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
        try {
            v.set( size, Cons.NIL );
            fail();
        }
        catch ( ScreamException e ) {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, e.getCode() );
        }
    }

    @Test
    public void copyYes() throws Exception
    {
        var a = new FirstClassObject[]
        {
            TestUtil.i1,
            TestUtil.i2
        };

        var vCopy = new Vector( a );
        assertEquals( a.length, vCopy.size() );
        vCopy.set( 0, TestUtil.s313 );
        assertEquals( TestUtil.s313, vCopy.get( 0 ) );
    }

    @Test
    public void copyNo() throws Exception
    {
        var a = new FirstClassObject[]
        {
            TestUtil.i1,
            TestUtil.i2
        };

        var vCopy = new Vector( a, false );
        assertEquals( a.length, vCopy.size() );
        vCopy.set( 0, TestUtil.s313 );
        assertEquals( TestUtil.s313, vCopy.get( 0 ) );
        assertEquals( TestUtil.s313, a[0] );
    }

    @Test
    public void vector_length_Test() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        var result = se.eval(
                """
                (define v #(1 2 3))
                (vector-length v)
                """ );
        assertEquals( result, TestUtil.i3 );
    }

    @Test
    public void vector_ref_Test() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        var result = (FirstClassObject)se.eval(
                """
                (define v #(1 2 3))
                (vector-ref v 0)
                """ );
        assertTrue( TestUtil.i1.equal( result ) );
    }

    @Test
    public void vector_set_Test() throws Exception
    {
        SchemeInterpreter2 si = new SchemeInterpreter2();
        var se = si.getScriptEngine();

        var result = (FirstClassObject)se.eval(
                """
                (define v #(1 2 3))
                (vector-set! v 0 313)
                (vector-ref v 0)
                """ );
        assertTrue( TestUtil.i313.equal( result ) );
    }
}
