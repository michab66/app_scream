package de.michab.scream.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Operation;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.SchemeInteger;

public class SchemeObjectTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                object
                """ );
        assertInstanceOf( Operation.class, result );
    }

    public static class ObjectTest
    {
        public int attributeA = 0;
        public int attributeB = 0;
        public ObjectTest()
        {
            this( 0, 0 );
        }
        public ObjectTest( int a, int b )
        {
            attributeA = a;
            attributeB = b;
        }
        public int sum()
        {
            return attributeA + attributeB;
        }
        public int sumPlus( int plus )
        {
            return sum() + plus;
        }
        public static int donaldian()
        {
            return 313;
        }
        public static int donaldianPlus( int plus )
        {
            return 313 + plus;
        }
    }

    @Test
    public void createClass() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                o
                """ );
        assertInstanceOf( SchemeObject.class, result );
        Class<?> ih = (Class<?>)result.toJava();
        assertEquals( "de.michab.scream.binding.SchemeObjectTest$ObjectTest", ih.getName() );
    }
    @Test
    public void isClassAnObject() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                (object? o)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }
    @Test
    public void isNumberAnObject() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (object? 1)
                """ );
        assertEquals( SchemeBoolean.F, result );
    }
    @Test
    public void isInstanceAnObject() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 313 0)))
                (object? o)
                """ );
        assertEquals( SchemeBoolean.T, result );
    }
    @Test
    public void create313() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 313 0)))
                o
                """ );
        assertInstanceOf( SchemeObject.class, result );
        ObjectTest ih = (ObjectTest)result.toJava();
        assertEquals( 313, ih.sum() );
    }
    @Test
    public void createDefault() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest)))
                o
                """ );
        assertInstanceOf( SchemeObject.class, result );
        ObjectTest ih = (ObjectTest)result.toJava();
        assertEquals( 0, ih.sum() );
    }
    @Test
    public void getAttribute() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 313 0)))
                (o attributeA)
                """ );
        assertInstanceOf( SchemeInteger.class, result );
        assertEqualq( i313, result );
    }
    @Test
    public void setAttribute() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest)))
                (o attributeB 121)
                o
                """ );
        assertInstanceOf( SchemeObject.class, result );
        ObjectTest ih = (ObjectTest)result.toJava();
        assertEquals( 121, ih.attributeB );
    }
    @Test
    public void callOperationNoParams() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 311 2)))
                (o (sum))
                """ );
        assertInstanceOf( SchemeInteger.class, result );
        assertEqualq( i313, result );
    }
    @Test
    public void callOperation() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 310 2)))
                (o (sumPlus 1))
                """ );
        assertInstanceOf( SchemeInteger.class, result );
        assertEqualq( i313, result );
    }
    @Test
    public void callStaticOperationNoParam() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                (o (donaldian))
                """ );
        assertInstanceOf( SchemeInteger.class, result );
        assertEqualq( i313, result );
    }
    @Test
    public void callStaticOperation() throws Exception
    {
        ScreamEvaluator se = scriptEngine();

        var result = se.evalFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                (o (donaldianPlus 1))
                """ );
        assertInstanceOf( SchemeInteger.class, result );
        assertEqualq( i(314), result );
    }
}
