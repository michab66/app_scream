/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Operation;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.util.Scut;

public class SchemeObjectTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        var t = makeTester();

        Scut.as( Operation.class,
                t.execute(
                        "object" ) );
        Scut.as( Procedure.class,
                t.execute(
                        "object?" ) );
        Scut.as( Operation.class,
                t.execute(
                        "make-object" ) );
    }

    public static class ObjectTest
    {
        public static final int
            publicStaticFinalZero = 0;
        protected static final int
            protectedStaticFinalZero = 0;
        public static int
            publicStaticZero = 0;

        public final int
            publicFinalZero = 0;

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

        public int getA()
        {
            return attributeA;
        }
        public void setA( int a )
        {
            attributeA = a;
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
    public void make_object__createClass() throws Exception
    {
        var t = makeTester();

        var result = t.execute(
                """
                (define o
                   (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                o
                """ );

        assertInstanceOf( SchemeObject.class, result );

        Class<?> cl =
                (Class<?>)result.toJava();
        assertEquals(
                "de.michab.scream.binding.SchemeObjectTest$ObjectTest",
                cl.getName() );


    }

    @Test
    public void objectq() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(object? (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 313 0)))",
                Bool.T );
        t.expectFco(
                "(object? (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))",
                Bool.T );
        t.expectFco(
                "(object? (make-object java.lang.String))",
                Bool.T );
        t.expectFco(
                "(object? 1)",
                Bool.F );
        t.expectFco(
                "(object? '())",
                Bool.F );
        t.expectFco(
                "(object? 'symbol)",
                Bool.F );
        t.expectError(
                "(object?)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                "(object? 1 2)",
                Code.WRONG_NUMBER_OF_ARGUMENTS );
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
    public void callOperationNoParams() throws Exception
    {
        expectFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 311 2)))
                (o (sum))
                """,
                i313 );
    }

    @Test
    public void callOperation() throws Exception
    {
        expectFco(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 310 2)))
                (o (sumPlus 1))
                """,
                i313 );
    }

    @Test
    public void callStaticOperationNoParam() throws Exception
    {
        expectFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                (o (donaldian))
                """,
                i313 );
    }

    @Test
    public void callStaticOperation() throws Exception
    {
        expectFco(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                (o (donaldianPlus 1))
                """,
                i(314) );
    }

    @Test
    public void accessStaticMembers() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define o
                    (make-object de.michab.scream.binding.SchemeObjectTest$ObjectTest))
                """ );
        t.expectFco(
                "(o publicStaticFinalZero)",
                i(0) );
        t.expectError(
                "(o publicStaticFinalZero 1)",
                Code.CANNOT_MODIFY_CONSTANT );
        t.expectError(
                "(o protectedStaticFinalZero)",
                Code.FIELD_NOT_FOUND );
        t.expectFco(
                "(o publicStaticZero)",
                i(0) );
        t.expectFco(
                "(o publicStaticZero 8)",
                i(8) );
        t.expectFco(
                "(o publicStaticZero)",
                i(8) );
    }

    @Test
    public void accessInstanceMembers() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define o
                    (make-object (de.michab.scream.binding.SchemeObjectTest$ObjectTest 1 2)))
                """ );
        t.expectFco(
                "(o attributeA)",
                i(1) );
        t.expectFco(
                "(o attributeB)",
                i(2) );
        t.expectFco(
                "(o publicFinalZero)",
                i(0) );

        t.expectFco(
                "(o attributeA 3)",
                i(3) );
        t.expectFco(
                "(o attributeA)",
                i(3) );

        t.expectFco(
                "(o attributeB 4)",
                i(4) );
        t.expectFco(
                "(o attributeB)",
                i(4) );

        t.expectError(
                "(o publicFinalZero 13)",
                Code.CANNOT_MODIFY_CONSTANT );
    }
}
