/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
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

    @Test
    public void objectq() throws Exception
    {
        var t = makeTester();

        // Class object.
        t.expectFco(
                "(object? (make-object java.lang.StringBuilder))",
                Bool.T );
        // Instance.
        t.expectFco(
                "(object? (make-object (java.lang.StringBuilder)))",
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

    public static class Cl_StaticMembers
    {
        public static final int
            publicStaticFinalZero = 0;
        protected static final int
            protectedStaticFinalZero = 0;
        public static int
            publicStaticZero = 0;
        public static int get313()
        {
            return 313;
        }
        public static int add( int a, int b )
        {
            return a + b;
        }
    }

    @Test
    public void accessStaticMembers() throws Exception
    {
        var t = makeTester();

        t.execute(
                """
                (define Cl_StaticMembers
                    (make-object de.michab.scream.binding.SchemeObjectTest$Cl_StaticMembers))
                """ );
        t.expectFco(
                "(Cl_StaticMembers publicStaticFinalZero)",
                i(0) );
        t.expectError(
                "(Cl_StaticMembers publicStaticFinalZero 1)",
                Code.CANNOT_MODIFY_CONSTANT );
        t.expectError(
                "(Cl_StaticMembers protectedStaticFinalZero)",
                Code.FIELD_NOT_FOUND );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero)",
                i(0) );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero 8)",
                i(8) );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero)",
                i(8) );
        t.expectFco(
                "(Cl_StaticMembers (get313))",
                i(313) );
        t.expectFco(
                "(Cl_StaticMembers (add 310 3))",
                i(313) );
    }

    @Test
    public void make_object__createClass() throws Exception
    {
        var t = makeTester();

        var result = t.execute(
                """
                (define o
                   (make-object java.lang.StringBuilder))
                o
                """ );

        assertInstanceOf( SchemeObject.class, result );

        Class<?> cl =
                (Class<?>)result.toJava();
        assertEquals(
                "java.lang.StringBuilder",
                cl.getName() );
    }

    @Test
    public void make_object__createInstanceDefault() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        var result = t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                StringBuilder
                """ );

        assertInstanceOf( SchemeObject.class, result );

        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
    }

    @Test
    public void make_object__createInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        var result = t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder \"scream\")))
                StringBuilder
                """ );

        assertInstanceOf( SchemeObject.class, result );

        t.expectFco(
                "(StringBuilder (toString))",
                str("scream") );
    }

    @Test
    public void make_object__useInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                """ );
        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
        t.execute(
                "(StringBuilder (append \"scream\"))" );
        t.expectFco(
                "(StringBuilder (toString))",
                str( "scream" ) );
        t.execute(
                "(StringBuilder (append 313.))" );
        t.expectFco(
                "(StringBuilder (toString))",
                str( "scream313.0" ) );
    }

    @Test
    public void make_object__useInstance_generic() throws Exception
    {
        var t = makeTester();

        // Generic class without default constructor.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair "stan" "ollie")))
                """ );
        t.expectFco(
                "(o left)",
                str("stan") );
        t.expectFco(
                "(o right)",
                str("ollie") );

        // Generic class without default constructor.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair 121 313)))
                """ );
        t.expectFco(
                "(o left)",
                i(121) );
        t.expectFco(
                "(o right)",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair "stan" 313)))
                """ );
        t.expectFco(
                "(o left)",
                str("stan") );
        t.expectFco(
                "(o right)",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair 'lambda 313)))
                """ );
        // Symbol converted to string.
        t.expectFco(
                "(o left)",
                str("lambda") );
        t.expectFco(
                "(o right)",
                i(313) );

        // Storing a function in the pair.
        t.execute(
                """
                (define o
                   (make-object (org.smack.util.Pair + 313)))
                """ );
        t.expectFco(
                "((o left) (o right) (o right))",
                i(626) );
    }


    @Test
    @Disabled
    public void todo_StringBuilder_useInstance() throws Exception
    {
        var t = makeTester();

        // Simple class with default constructor.
        t.execute(
                """
                (define StringBuilder
                   (make-object (java.lang.StringBuilder)))
                """ );
        t.expectFco(
                "(StringBuilder (toString))",
                str("") );
        t.execute(
                "(StringBuilder (append 313))" );

        // Wrong method selected: Returned value is 313.0.
        t.expectFco(
                "(StringBuilder (toString))",
                str( "313" ) );
    }
}

