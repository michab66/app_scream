/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.util.Objects;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.Real;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.fcos.Vector;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * An instance of this class boxes an entity from the Java object system,
 * i.e. an object or a class. This wraps the object and implements the
 * Scheme/Java call mapping.
 *
 * @author Michael G. Binz
 */
public class SchemeObject
    extends Syntax
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "object";

    /**
     * The managed instance.  May be {@code null} for classes.
     */
    private final Object _instance;

    /**
     * The instance's class adapter.
     */
    private final JavaClassAdapter _classAdapter;
    private final Class <?> _class;

    /**
     * Create a new SchemeObject for a given object.  In case the
     * passed object instance is {@code null} the resulting SchemeObject
     * represents a class.
     *
     * @param object The object to be wrapped by the new instance.
     * @param adapter The class adapter for the new instance.
     */
    private SchemeObject( java.lang.Object object, JavaClassAdapter adapter )
    {
        super( adapter.adapterFor().getName() );

        _instance = object;
        _class = adapter.adapterFor();

        _classAdapter = adapter;
    }

    /**
     * Create a new SchemeObject for a given object.
     *
     * @param object The object to be wrapped by the new instance.
     */
    private SchemeObject( java.lang.Object object )
    {
        this( object, JavaClassAdapter.make( object.getClass() ) );
    }

    /**
     * Wrap the passed object as a SchemeObject.  If null is passed then
     * null is returned.  If the passed object is already a SchemeObject
     * then this is returned.
     *
     * @param object The object to wrap.
     * @return A new SchemeObject or null if null was passed.
     */
    public static SchemeObject make( Object object )
    {
        if ( object == null )
            return null;

        if ( object instanceof SchemeObject )
            return (SchemeObject)object;

        return new SchemeObject( object );
    }

    public boolean isClass()
    {
        return _instance == null;
    }

    private static Thunk createClass(
            String classname,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return c.accept(
            new SchemeObject(
                null,
                JavaClassAdapter.make( classname ) ) );
    }

    private static Thunk createObject(
            String name,
            FirstClassObject[] ctorArgs,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Objects.requireNonNull(
                name );
        Objects.requireNonNull(
                ctorArgs );

        if ( name.length() == 0 )
            throw RuntimeX.mIllegalArgument( name );

        var result = JavaClassAdapter.createInstance(
                name,
                ctorArgs  );

        // No conversion to Fco.
        return c.accept(
                result instanceof FirstClassObject ?
                        FirstClassObject.class.cast( result ) :
                            new SchemeObject( result ) );
    }

    private static Thunk _createObject(
            String className,
            Cons ctorArgs,
            Cont<FirstClassObject> c )
    {
        return () -> createObject(
                className,
                Cons.asArray( ctorArgs ),
                c );
    }

    private static Thunk call(
            SchemeObject instance,
            String methodSpec,
            FirstClassObject[] args,
            Cont<SchemeObject> c )
                    throws RuntimeX
    {
        if ( methodSpec.length() == 0 )
            throw RuntimeX.mIllegalArgument( methodSpec );

        var classAdapter = JavaClassAdapter.make( instance._class );

        // No conversion to Fco.
        return c.accept(
                SchemeObject.make(
                        classAdapter.call(
                                instance,
                                methodSpec,
                                args  ) ) );
    }

    private Thunk _call(
            String methodName,
            Cons arguments,
            Cont<FirstClassObject> c )
    {
        return () -> call(
                this,
                methodName,
                Cons.asArray( arguments ),
                result -> _convertJava2Scream( result, c ) );
    }

    /**
     * @return The wrapped Java object.
     */
    @Override
    public Object toJava()
    {
        return _instance;
    }
    static public Object toJava( SchemeObject object )
    {
        if ( object == null )
            return object;

        return object.toJava();
    }

    @Override
    protected Thunk _executeImpl(
            Environment e,
            Cons args,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        long argsLen =
                checkArgumentCount( 1, 2, args );

        var args0 = args.listRef( 0 );

        if ( argsLen == 1 && FirstClassObject.is( Cons.class, args0 ) )
        {
            return invoke(
                    e,
                    Scut.asNotNil( Cons.class, args0 ),
                    c );
        }

        if ( argsLen == 1 && args0 instanceof SchemeString )
        {
            return processAttributeGet(
                    ((SchemeString)args0).getValue(),
                    c );
        }

        // TODO expect a string and throw type error.
        if ( argsLen == 2 && args0 instanceof Symbol )
        {
            Symbol symbol = (Symbol)args0;
            var args1 = args.listRef( 1 );

            return Primitives._eval(
                    e,
                    args1,
                    fco -> _setAttribute( symbol, fco, c ) );
        }

        throw RuntimeX.mInternalError( SchemeObject.class.toString() );
    }

    /**
     * Converts a Java object back into Scream's type system.
     *
     * @param object The object to be boxed.
     * @return The object representing the box.
     */
    static FirstClassObject convertJava2Scream( java.lang.Object object )
    {
        // Java nulls are mapped into NIL.
        if ( null == object )
            return Cons.NIL;

        // Test for array types...
        if ( object.getClass().isArray() )
            return convertJavaArray2ScreamVector( object );

        // Test for all primitive types supported by Java.
        if ( object instanceof java.lang.Integer ||
                object instanceof java.lang.Byte ||
                object instanceof java.lang.Short ||
                object instanceof java.lang.Long )
            return Int.createObject( ((java.lang.Number)object).longValue() );

        if ( object instanceof java.lang.Double ||
                object instanceof java.lang.Float )
            return Real.createObject( ((java.lang.Number)object).doubleValue() );

        if ( object instanceof java.lang.Character )
            return SchemeCharacter.createObject( ((java.lang.Character)object).charValue() );

        if ( object instanceof String )
            return SchemeString.make( (String)object );

        if ( object instanceof java.lang.Boolean )
            return Bool.createObject( ((java.lang.Boolean)object).booleanValue() );

        // This is needed for tightly integrated classes that know about Scream's
        // internal type system.
        if ( object instanceof FirstClassObject )
            return (FirstClassObject)object;

        // Everything else must be a Java-native type.
        return SchemeObject.make( object );
    }

    private static Thunk _convertJava2Scream(
            SchemeObject object,
            Cont<FirstClassObject> c )
    {
        return ScreamEvaluator.CONT.get().toCont(
                () -> convertJava2Scream( SchemeObject.toJava( object ) ),
                c );
    }

    /**
     * Converts a Java array into a scream vector.  The passed object must be
     * an array, i.e. {@code Array.isArray()} must return true for it.  This
     * is not checked inside this method.
     *
     * @param object The object to convert to a vector.
     * @return The resulting vector.
     */
    private static Vector convertJavaArray2ScreamVector( java.lang.Object object )
    {
        FirstClassObject[] vector =
                new FirstClassObject[ Array.getLength( object ) ];
        for ( int i = vector.length -1 ; i >= 0 ; i-- )
            vector[i] = convertJava2Scream( Array.get( object, i ) );
        return new Vector( vector, false );
    }

    /**
     * Processes a procedure invocation.
     *
     * @param env The environment used for the evaluation of the argument list.
     * @param list The arguments used for the invocation.
     * @return The result of the procedure invocation.
     * @throws RuntimeX In case there where access errors.
     */
    private Thunk invoke(
            Environment env,
            Cons list,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        var string = Scut.as(
                SchemeString.class, list.getCar() );
        var rest = Scut.as(
                Cons.class, list.getCdr() );

        return Primitives._evalCons(
                env,
                rest,
                evaluated -> _call(
                        string.getValue(),
                        evaluated,
                        c ) );
    }

    /**
     * Get/Read an attribute from the passed object.
     *
     * @param attribute The attribute to read.
     * @param c The continuation receiving the attribute's value.
     * @return A thunk.
     * @throws RuntimeX In case the attribute could not be read.
     */
    private Thunk processAttributeGet( String attribute, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        try
        {
            return c.accept( convertJava2Scream(
                    _classAdapter.getField( attribute ).get( _instance ) ) );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( attribute );
        }
    }

    /**
     * Sets an attribute on the passed object.
     *
     * @param attribute The attribute to set.
     * @param value The value to set.
     * @return The attribute's new value.
     * @throws RuntimeX In case the attribute could not be set.
     */
    private Thunk processAttributeSet(
            Symbol attribute,
            FirstClassObject value,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        return c.accept(
                _classAdapter.setField(
                        attribute.toString(),
                        _instance,
                        value ) );
    }

    private Thunk _setAttribute(
            Symbol attribute,
            FirstClassObject value,
            Cont<FirstClassObject> c )
    {
        return () -> processAttributeSet( attribute, value, c );
    }

    /**
     * Test for equality based on the embedded instance.  If we have no instance
     * which is the case for class representatives test if the same
     * {@code JavaClassAdapter} is used.
     *
     * @param other The object to compare.
     * @return {@code True} if the reference passed into this call is eqv to
     *         this object.
     */
    @Override
    public boolean eqv( FirstClassObject other )
    {
        try
        {
            SchemeObject otherSo = (SchemeObject)other;

            if ( Objects.equals( _instance, otherSo._instance ) )
                if ( _instance == null )
                    return Objects.equals( _class, otherSo._class );
            return true;
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }

    /**
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return "@%s:class=%s, instance=%s".formatted(
                TYPE_NAME,
                _class,
                _instance);
    }

    /**
     * Clone this scheme object.  Makes a deep copy.
     *
     * @return The cloned object.
     * @see java.lang.Object#clone
     */
    @Override
    public SchemeObject copy()
    {
        throw new InternalError( getClass().getSimpleName() );
    }

    /**
     * Create an object or class representation.
     *
     * classname is a symbol.
     *
     * (make-object classname) -> class
     * (make-object (classname ...) -> instance ctor
     */
    static private Syntax constructObjectSyntax = new Syntax( "make-object" )
    {
        @Override
        protected Thunk _executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 1, args );

            FirstClassObject argument = args.getCar();

            if ( argument instanceof SchemeString )
                return createClass( (String)argument.toJava(), c );

            Cons cons = Scut.as(
                    Cons.class,
                    argument );
            Cons arguments = Scut.as(
                    Cons.class,
                    cons.getCdr() );

            SchemeString name = Scut.as(
                    SchemeString.class,
                    cons.getCar() );

            return Primitives._evalCons(
                    e,
                    arguments,
                    evaluated ->
                    _createObject(
                            name.getValue(),
                            evaluated,
                            object
                              -> {  return c.accept( convertJava2Scream( object ) ); }) );
        }
    };

    /**
     * (scream:java:make-instance "ctor-spec" ...)
     */
    static private Procedure scream_java_make_instance( Environment e )
    {
        return new Procedure( "scream:java:make-instance", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        Integer.MAX_VALUE,
                        args );

                SchemeString ctor_spec =
                        Scut.asNotNil( SchemeString.class, args.getCar() );
                Cons parameters =
                        Scut.as( Cons.class, args.getCdr() );

                return createObject(
                        ctor_spec.toJava(),
                        Cons.asArray( parameters),
                        c );
            }
        };
    }

    /**
     * (scream:java:make-class "class-name")
     *
     * Creates an object representing a class.
     */
    static private Procedure scream_java_make_class( Environment e )
    {
        return new Procedure( "scream:java:make-class", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        args );

                SchemeString classname =
                        Scut.asNotNil( SchemeString.class, args.getCar() );

                return createClass( classname.toJava(), c );
            }
        };
    }

    /**
     * (scream:java:make-class-object "class-name")
     *
     * Creates an object representing a class.
     */
    static private Procedure scream_java_make_class_object( Environment e )
    {
        return new Procedure( "scream:java:make-class-object", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        args );

                SchemeString classname =
                        Scut.asNotNil( SchemeString.class, args.getCar() );


                var x = SchemeObject.make(
                        JavaClassAdapter.make( classname.toJava() ).adapterFor() );

                return c.accept( x );
            }
        };
    }

    /**
     * (scream:java:call object "meth-spec" ...)
     *
     * Calls the operation specified by meth-spec on object.
     * If the object represents a class then the static method is called.
     */
    static private Procedure scream_java_call( Environment e )
    {
        return new Procedure( "scream:java:call", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        2,
                        Integer.MAX_VALUE,
                        args );

                SchemeObject instance =
                        Scut.asNotNil( SchemeObject.class, args.getCar() );
                args =
                        Scut.as( Cons.class, args.getCdr() );
                SchemeString arg_spec =
                        Scut.asNotNil( SchemeString.class, args.getCar() );
                Cons parameters =
                        Scut.as( Cons.class, args.getCdr() );

                return call(
                        instance,
                        arg_spec.getValue(),
                        Cons.asArray( parameters),
                        so -> c.accept( so ) );
            }
        };
    }

    /**
     * (scream:java:to-fco object)
     *
     * Converts the passed object back into a Scheme-type.
     * If the passed object is already a Scheme-type the object is returned
     * unmodified.
     */
    static private Procedure scream_java_to_fco( Environment e )
    {
        return new Procedure( "scream:java:to-fco", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                if ( ! Cons.isProper( args ) )
                    throw RuntimeX.mExpectedProperList( args );

                checkArgumentCount(
                        1,
                        args );

                var a0 = args.getCar();

                if ( ! FirstClassObject.is( SchemeObject.class, a0 ) )
                    return c.accept( a0 );

                SchemeObject object =
                        Scut.asNotNil( SchemeObject.class, a0 );

                return c.accept( convertJava2Scream( object.toJava() ) );
            }
        };
    }

    /**
     * (object obj) -- Wraps the passed first class object with a scheme object.
     */
    static private Procedure wrapObjectProcedure( Environment e )
    {
        return new Procedure( "object", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return c.accept(
                        SchemeObject.make(
                                args.getCar() ) );
            }
        };
    }

    /**
     * (object? obj)
     */
    static private Procedure objectPredicateProcedure( Environment e )
    {
        return new Procedure( "object?", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return () -> c.accept(
                        Bool.createObject( args.getCar() instanceof SchemeObject ) );
            }
        };
    }

    /**
     * Object operations setup.
     *
     * @param tle The environment to extend.
     * @return The extended environment.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( objectPredicateProcedure(tle) );
        tle.setPrimitive( wrapObjectProcedure( tle ) );
        tle.setPrimitive( constructObjectSyntax );

        tle.setPrimitive( scream_java_make_instance( tle ) );
        tle.setPrimitive( scream_java_call( tle ) );
        tle.setPrimitive( scream_java_make_class( tle ) );
        tle.setPrimitive( scream_java_make_class_object( tle ) );
        tle.setPrimitive( scream_java_to_fco( tle ) );
        return tle;
    }
}
