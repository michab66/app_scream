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
import org.smack.util.StringUtil;

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
                "(object? (make-object \"java.lang.StringBuilder\"))",
                Bool.T );
        // Instance.
        t.expectFco(
                "(object? (make-object (\"java.lang.StringBuilder\")))",
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
                    (make-object "de.michab.scream.binding.SchemeObjectTest$Cl_StaticMembers"))
                """ );
        t.expectFco(
                "(Cl_StaticMembers \"publicStaticFinalZero\")",
                i(0) );
        t.expectError(
                "(Cl_StaticMembers publicStaticFinalZero 1)",
                Code.CANNOT_MODIFY_CONSTANT );
        t.expectError(
                "(Cl_StaticMembers \"protectedStaticFinalZero\")",
                Code.FIELD_NOT_FOUND );
        t.expectFco(
                "(Cl_StaticMembers \"publicStaticZero\")",
                i(0) );
        t.expectFco(
                "(Cl_StaticMembers publicStaticZero 8)",
                i(8) );
        t.expectFco(
                "(Cl_StaticMembers \"publicStaticZero\")",
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
                   (make-object \"java.lang.StringBuilder\"))
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
                   (make-object ("java.lang.StringBuilder")))
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
                   (make-object (\"java.lang.StringBuilder:java.lang.String\" \"scream\")))
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
                   (make-object ("java.lang.StringBuilder")))
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
                   (make-object (\"org.smack.util.Pair:java.lang.Object,java.lang.Object\" "stan" "ollie")))
                """ );
        t.expectFco(
                "(o \"left\")",
                str("stan") );
        t.expectFco(
                "(o \"right\")",
                str("ollie") );

        // Generic class without default constructor.
        t.execute(
                """
                (define o
                   (make-object (\"org.smack.util.Pair:java.lang.Object,java.lang.Object\" 121 313)))
                """ );
        t.expectFco(
                "(o \"left\")",
                i(121) );
        t.expectFco(
                "(o \"right\")",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (\"org.smack.util.Pair:java.lang.Object,java.lang.Object\" "stan" 313)))
                """ );
        t.expectFco(
                "(o \"left\")",
                str("stan") );
        t.expectFco(
                "(o \"right\")",
                i(313) );

        t.execute(
                """
                (define o
                   (make-object (\"org.smack.util.Pair:java.lang.Object,java.lang.Object\" 'lambda 313)))
                """ );
        // Symbol converted to string.
        t.expectFco(
                "(o \"left\")",
                str("lambda") );
        t.expectFco(
                "(o \"right\")",
                i(313) );

        // Storing a function in the pair.
        t.execute(
                """
                (define o
                   (make-object (\"org.smack.util.Pair:java.lang.Object,java.lang.Object\" + 313)))
                """ );
        t.expectFco(
                "((o \"left\") (o \"right\") (o \"right\"))",
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

    public static class MakeInstance
    {
        public String message = StringUtil.EMPTY_STRING;

        public MakeInstance()
        {
        }
        public MakeInstance( String s )
        {
            message = "String:" + s;
        }
        public MakeInstance( String s1, String s2 )
        {
            message = "String:" + s1 + ":String:" + s2;
        }
        public MakeInstance( float f )
        {
            message = "float:" + f;
        }
        public MakeInstance( Float f )
        {
            message = "Float:" + f;
        }
        public MakeInstance( double d )
        {
            message = "double:" + d;
        }
        public MakeInstance( float f, double d )
        {
            message = "float:" + f + ":double:" + d;
        }
        public MakeInstance( double d, float f )
        {
            message = "double:" + d + ":float:" + f;
        }
        public MakeInstance( boolean b )
        {
            message = "boolean:" + b;
        }
        public MakeInstance( byte b )
        {
            message = "byte:" + b;
        }
        public MakeInstance( short b )
        {
            message = "short:" + b;
        }
        public MakeInstance( int b )
        {
            message = "int:" + b;
        }
        public MakeInstance( long b )
        {
            message = "long:" + b;
        }
    }

    /**
     * Support operation.
     *
     * @param ctorTypes
     * @param ctorArgs
     * @return
     */
    private String makeInstanceScript( String ctorTypes, String ctorArgs )
    {
        return String.format(
                """
                (
                  (scream:java:make-instance
                    "de.michab.scream.binding.SchemeObjectTest$MakeInstance:%s"
                    %s)
                  "message"
                )
                """,
                ctorTypes,
                ctorArgs );
    }

    @Test
    public void make_instance() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                makeInstanceScript(
                        StringUtil.EMPTY_STRING,
                        StringUtil.EMPTY_STRING ),
                str( StringUtil.EMPTY_STRING ) );

        t.expectFco(
                makeInstanceScript(
                        "java.lang.String",
                        "\"x\""),
                str( "String:x" ) );

        //
        // float / Float
        //
        t.expectFco(
                makeInstanceScript(
                        "float",
                        "1.0"),
                str( "float:1.0" ) );
        t.expectFco(
                makeInstanceScript(
                        "java.lang.Float",
                        "1.0"),
                str( "Float:1.0" ) );

        //
        // double
        //
        t.expectFco(
                makeInstanceScript(
                    "double",
                    "1.0"),
                str( "double:1.0" ) );

        t.expectFco(
                makeInstanceScript( "float,double",
                "1.0 2.0"),
                str( "float:1.0:double:2.0" ) );

        t.expectFco(
                makeInstanceScript(
                        "double,float",
                        "1.0 2.0" ),
                str( "double:1.0:float:2.0" ) );

        //
        // boolean
        //
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "#t" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "313" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "3.14159265" ),
                str( "boolean:true" ) );
        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "'()" ),
                str( "boolean:true" ) );

        t.expectFco(
                makeInstanceScript(
                        "boolean",
                        "#f" ),
                str( "boolean:false" ) );

        //
        // byte
        //
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "10" ),
                str( "byte:10" ) );
        t.expectFco(
                makeInstanceScript(
                        "byte",
                        "-1" ),
                str( "byte:-1" ) );
        // overflow
        t.expectError(
                makeInstanceScript(
                        "byte",
                        "" + (Byte.MAX_VALUE + 1) ),
                Code.RANGE_EXCEEDED );
        t.expectError(
                makeInstanceScript(
                        "byte",
                        "" + (Byte.MIN_VALUE - 1) ),
                Code.RANGE_EXCEEDED );

        //
        // short
        //
        t.expectFco(
                makeInstanceScript(
                        "short",
                        "10" ),
                str( "short:10" ) );
        t.expectFco(
                makeInstanceScript(
                        "short",
                        "-10" ),
                str( "short:-10" ) );
        // overflow
        t.expectError(
                makeInstanceScript(
                        "short",
                        "" + (Short.MAX_VALUE + 1) ),
                Code.RANGE_EXCEEDED );
        t.expectError(
                makeInstanceScript(
                        "short",
                        "" + (Short.MIN_VALUE - 1) ),
                Code.RANGE_EXCEEDED );

        //
        // int
        //
        t.expectFco(
                makeInstanceScript(
                        "int",
                        "-10" ),
                str( "int:-10" ) );
        t.expectError(
                makeInstanceScript(
                        "int",
                        "" + (Integer.MAX_VALUE + 1L ) ),
                Code.RANGE_EXCEEDED );
        t.expectError(
                makeInstanceScript(
                        "int",
                        "" + (Integer.MIN_VALUE - 1L ) ),
                Code.RANGE_EXCEEDED );

        //
        // long
        //
        t.expectFco(
                makeInstanceScript(
                        "long",
                        "-10" ),
                str( "long:-10" ) );

        t.expectError(
                """
                (
                  (scream:java:make-instance
                    "de.michab.scream.binding.SchemeObjectTest$MakeInstance:float,double"
                    1.0)
                  message
                )
                """,
                Code.WRONG_NUMBER_OF_ARGUMENTS );

        t.expectError(
                """
                (scream:java:make-instance "MakeInstance:")
                """,
                Code.CLASS_NOT_FOUND );
    }

    public static class CallInstance
    {
        public CallInstance()
        {
        }
        public String mcall()
        {
            return StringUtil.EMPTY_STRING;
        }
        public String mcall( String s )
        {
            return "String:" + s;
        }
        public String mcall( String s1, String s2 )
        {
            return "String:" + s1 + ":String:" + s2;
        }
        public String mcall( float f )
        {
            return "float:" + f;
        }
        public String mcall( Float f )
        {
            return "Float:" + f;
        }
        public String mcall( double d )
        {
            return "double:" + d;
        }
        public String mcall( float f, double d )
        {
            return "float:" + f + ":double:" + d;
        }
        public String mcall( double d, float f )
        {
            return "double:" + d + ":float:" + f;
        }
        public String mcall( boolean b )
        {
            return "boolean:" + b;
        }
        public String mcall( byte b )
        {
            return "byte:" + b;
        }
        public String mcall( short b )
        {
            return "short:" + b;
        }
        public String mcall( int b )
        {
            return "int:" + b;
        }
        public String mcall( long b )
        {
            return "long:" + b;
        }
        public int mcall( int[] integers )
        {
            int result = 0;

            for ( var c : integers )
                result += c;

            return result;
        }
        public long mcall( long ... longs )
        {
            long result = 0;

            for ( var c : longs )
                result += c;

            return result;
        }
    }
    /**
     * Support operation.
     *
     * @param ctorTypes
     * @param ctorArgs
     * @return
     */
    private String scream_java_call( String callTypes, String callArgs )
    {
        return String.format(
                """
                (let
                  (
                    (i (scream:java:make-instance
                         "de.michab.scream.binding.SchemeObjectTest$CallInstance:"))
                  )
                  ((scream:java:call i "mcall:%s" %s) (toString))
                )
                """,
                callTypes,
                callArgs );
    }
    private String object_call( String callTypes, String callArgs )
    {
        return String.format(
                """
                (let
                  (
                    (i (scream:java:make-instance
                         "de.michab.scream.binding.SchemeObjectTest$CallInstance:"))
                  )
                  (i ("mcall:%s" %s))
                )
                """,
                callTypes,
                callArgs );
    }

    @Test
    public void call_instance() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                scream_java_call(
                        StringUtil.EMPTY_STRING,
                        StringUtil.EMPTY_STRING ),
                str( StringUtil.EMPTY_STRING ) );
        t.expectFco(
                object_call(
                        StringUtil.EMPTY_STRING,
                        StringUtil.EMPTY_STRING ),
                str( StringUtil.EMPTY_STRING ) );

        t.expectFco(
                scream_java_call(
                        "java.lang.String",
                        "\"x\""),
                str( "String:x" ) );
        t.expectFco(
                object_call(
                        "java.lang.String",
                        "\"x\""),
                str( "String:x" ) );

        //
        // float / Float
        //
        t.expectFco(
                scream_java_call(
                        "float",
                        "1.0"),
                str( "float:1.0" ) );
        t.expectFco(
                object_call(
                        "float",
                        "1.0"),
                str( "float:1.0" ) );

        t.expectFco(
                scream_java_call(
                        "java.lang.Float",
                        "1.0"),
                str( "Float:1.0" ) );
        t.expectFco(
                object_call(
                        "java.lang.Float",
                        "1.0"),
                str( "Float:1.0" ) );

        //
        // double
        //
        t.expectFco(
                scream_java_call(
                    "double",
                    "1.0"),
                str( "double:1.0" ) );
        t.expectFco(
                object_call(
                    "double",
                    "1.0"),
                str( "double:1.0" ) );

        t.expectFco(
                scream_java_call( "float,double",
                "1.0 2.0"),
                str( "float:1.0:double:2.0" ) );
        t.expectFco(
                object_call( "float,double",
                "1.0 2.0"),
                str( "float:1.0:double:2.0" ) );

        t.expectFco(
                scream_java_call(
                        "double,float",
                        "1.0 2.0" ),
                str( "double:1.0:float:2.0" ) );
        t.expectFco(
                object_call(
                        "double,float",
                        "1.0 2.0" ),
                str( "double:1.0:float:2.0" ) );

        //
        // boolean
        //
        t.expectFco(
                scream_java_call(
                        "boolean",
                        "#t" ),
                str( "boolean:true" ) );
        t.expectFco(
                object_call(
                        "boolean",
                        "#t" ),
                str( "boolean:true" ) );

        t.expectFco(
                scream_java_call(
                        "boolean",
                        "313" ),
                str( "boolean:true" ) );
        t.expectFco(
                object_call(
                        "boolean",
                        "313" ),
                str( "boolean:true" ) );

        t.expectFco(
                scream_java_call(
                        "boolean",
                        "3.14159265" ),
                str( "boolean:true" ) );
        t.expectFco(
                object_call(
                        "boolean",
                        "3.14159265" ),
                str( "boolean:true" ) );

        t.expectFco(
                scream_java_call(
                        "boolean",
                        "'()" ),
                str( "boolean:true" ) );
        t.expectFco(
                object_call(
                        "boolean",
                        "'()" ),
                str( "boolean:true" ) );

        t.expectFco(
                scream_java_call(
                        "boolean",
                        "#f" ),
                str( "boolean:false" ) );
        t.expectFco(
                object_call(
                        "boolean",
                        "#f" ),
                str( "boolean:false" ) );

        //
        // byte
        //
        t.expectFco(
                scream_java_call(
                        "byte",
                        "10" ),
                str( "byte:10" ) );
        // overflow
        t.expectFco(
                scream_java_call(
                        "byte",
                        "-1" ),
                str( "byte:-1" ) );
        // overflow
        t.expectError(
                scream_java_call(
                        "byte",
                        "256" ),
                Code.RANGE_EXCEEDED );
        // overflow
        t.expectError(
                scream_java_call(
                        "byte",
                        "257" ),
                Code.RANGE_EXCEEDED );

        //
        // short
        //
        t.expectFco(
                scream_java_call(
                        "short",
                        "10" ),
                str( "short:10" ) );
        t.expectFco(
                scream_java_call(
                        "short",
                        "-10" ),
                str( "short:-10" ) );

        //
        // int
        //
        t.expectFco(
                scream_java_call(
                        "int",
                        "-10" ),
                str( "int:-10" ) );
        t.expectFco(
                object_call(
                        "int",
                        "-10" ),
                str( "int:-10" ) );

        //
        // long
        //
        t.expectFco(
                scream_java_call(
                        "long",
                        "-10" ),
                str( "long:-10" ) );

        t.expectError(
                scream_java_call(
                        "double,double",
                        "1.0 2.0" ),
                Code.METHOD_NOT_FOUND );

        t.expectError(
                scream_java_call(
                        "double",
                        "1.0 2.0" ),
                Code.WRONG_NUMBER_OF_ARGUMENTS );

        t.expectError(
                scream_java_call(
                        "double",
                        StringUtil.EMPTY_STRING ),
                Code.WRONG_NUMBER_OF_ARGUMENTS );
        t.expectError(
                object_call(
                        "double",
                        StringUtil.EMPTY_STRING ),
                Code.WRONG_NUMBER_OF_ARGUMENTS );
    }

    public static class CallInstanceArrays
    {
        public CallInstanceArrays()
        {
        }
        public int intarray( int[] integers )
        {
            int result = 0;

            for ( var c : integers )
                result += c;

            return result;
        }
        public long longVariadic( long ... longs )
        {
            if ( longs == null )
                return -1;

            long result = 0;

            for ( var c : longs )
                result += c;

            return result;
        }
    }

    @Test
    public void call_instance_array() throws Exception
    {
        CallInstanceArrays cia = new CallInstanceArrays();
        cia.longVariadic();

        var t = makeTester();

        t.execute(
                """
                (define
                  i
                  (scream:java:make-instance
                         "de.michab.scream.binding.SchemeObjectTest$CallInstanceArrays:"))
                """ );
        // Call with a vector.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "intarray:int[]"
                    (vector 1 2 3))
                )
                """,
                i(6) );
        // Call with a list.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "intarray:int[]"
                    '(1 2 3))
                )
                """,
                i(6) );

        // Call variadic with no variadic elements.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "longVariadic:long[]")
                )
                """,
                i(-1) );

        // Call variadic with variadic elements.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "longVariadic:long[]"
                    1
                    2
                    3)
                )
                """,
                i(6) );

        // Call variadic with an explicit list.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "longVariadic:long[]"
                    '(1 2 3))
                )
                """,
                i(6) );

        // Call variadic with a vector.
        t.expectFco(
                """
                (scream:java:to-fco
                  (scream:java:call
                    i
                    "longVariadic:long[]"
                    #(1 2 3))
                )
                """,
                i(6) );
    }

    @Test
    public void call_class() throws Exception
    {
        var t = makeTester();

        t.execute( "(define math (scream:java:make-class \"java.lang.Math\"))" );

        t.expectFco(
                "(scream:java:to-fco (scream:java:call math \"sqrt:double\" 2))",
                d( 1.4142135623730951 ) );
    }

    @Test
    public void splitAt()
    {
        {
            var result = SchemeObject.splitAt( "a:b", ":" );
            assertEquals( 2, result.size() );
            assertEquals( "a", result.get( 0 ) );
            assertEquals( "b", result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( "a:", ":" );
            assertEquals( 2, result.size() );
            assertEquals( "a", result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( ":", ":" );
            assertEquals( 2, result.size() );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
        }
        {
            var result = SchemeObject.splitAt( "::", ":" );
            assertEquals( 3, result.size() );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 0 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 1 ) );
            assertEquals( StringUtil.EMPTY_STRING, result.get( 2 ) );
        }
        {
            var result = SchemeObject.splitAt( "aa:bb:cc", ":" );
            assertEquals( 3, result.size() );
            assertEquals( "aa", result.get( 0 ) );
            assertEquals( "bb", result.get( 1 ) );
            assertEquals( "cc", result.get( 2 ) );
        }
    }
}
