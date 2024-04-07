/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.logging.Logger;

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
 * e.g. an object or a class. This wraps the object and implements the
 * Scheme/Java call mapping in both directions.
 *
 * @author Michael G. Binz
 */
public class SchemeObject
    extends Syntax
{
    private static Logger LOG =
            Logger.getLogger( SchemeObject.class.getName() );

    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "object";

    /**
     * The instance that is managed.  Note that this must never be null.
     */
    private final java.lang.Object _delegate;
    private final Object _instance;

    /**
     * {@code True} if this object represents a Java class,
     * {@code false} otherwise.
     */
    private final boolean _isClass;

    /**
     * The instance's class adapter.
     */
    private final JavaClassAdapter _classAdapter;
    private final Class <?> _class;

    /**
     * Create a new SchemeObject for a given object.  In case the
     * passed object instance is {@code null} the
     * instance will be set to the class adapter's embedded class.
     * <p>
     * The resulting SchemeObject then represents a class.
     *
     * @param object The object to be wrapped by the new instance.
     * @param adapter The class adapter for the new instance.
     */
    private SchemeObject( java.lang.Object object, JavaClassAdapter adapter )
    {
        super( adapter.adapterFor().getName() );

        _instance = object;
        _class = adapter.adapterFor();

        if ( object == null )
        {
            _isClass = true;
            _delegate = adapter.adapterFor();
        }
        else
        {
            _isClass = false;

//            if ( object instanceof Class<?> )
//                LOG.severe( ((Class<?>)object).getName() );
            _delegate = object;
        }

        _classAdapter = adapter;
    }

    /**
     * Create a new SchemeObject for a given object.
     *
     * @param object The object to be wrapped by the new instance.
     */
    private SchemeObject( java.lang.Object object )
    {
        this( object, JavaClassAdapter.get( object.getClass() ) );
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
        var x = new SchemeObject(
                null,
                JavaClassAdapter.get( classname ) );

        return c.accept( x );
    }

    private static Thunk createObjectExt(
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

    private static Thunk _createObjectExt(
            String className,
            Cons ctorArgs,
            Cont<FirstClassObject> c )
    {
        return () -> createObjectExt(
                className,
                Cons.asArray( ctorArgs ),
                c );
    }

    private static Thunk callExt(
            SchemeObject instance,
            String methodSpec,
            FirstClassObject[] args,
            Cont<SchemeObject> c )
                    throws RuntimeX
    {
        if ( methodSpec.length() == 0 )
            throw RuntimeX.mIllegalArgument( methodSpec );

        var classAdapter =
                JavaClassAdapter.get(
                        instance._isClass ?
                                (Class<?>)instance.toJava() :
                                instance.toJava().getClass() );

        // No conversion to Fco.
        return c.accept(
                SchemeObject.make(
                        classAdapter.call(
                                instance,
                                methodSpec,
                                args  ) ) );
    }

    private Thunk _callExt(
            String methodName,
            Cons arguments,
            Cont<FirstClassObject> c )
    {
        return () -> callExt(
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
        return _delegate;
    }
    static public Object toJava( SchemeObject object )
    {
        if ( object == null )
            return object;

        return object._delegate;
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
            return processInvocation(
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

        if ( argsLen == 2 && args0 instanceof Symbol )
        {
            Symbol symbol = (Symbol)args0;
            var args1 = args.listRef( 1 );

            return Primitives._eval(
                    e,
                    args1,
                    fco -> _processAttributeSet( symbol, fco, c ) );
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
     * Converts a Java array into a scream vector.  The passed object must be an
     * array, i.e. {@code Array.isArray()} must return true for it.  This is
     * not checked inside this method.
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
    private Thunk processInvocation(
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
                evaluated -> _callExt(
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
                    _classAdapter.getField( attribute ).get( _delegate ) ) );
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
                        _delegate,
                        value ) );
    }

    private Thunk _processAttributeSet(
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

            return _delegate.equals( otherSo._delegate );
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
        return "@Object:%s=%s%nclass=%s, instance=%s".formatted(
                _delegate.getClass().getName(),
                _delegate,
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
                    _createObjectExt(
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

                return createObjectExt(
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

                return callExt(
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
     * (describe-object obj) -> #f
     *
     * TODO: return a more scheme like representation...
     */
    static private Procedure describeObjectProcedure( Environment e )
    {
        return new Procedure( "describe-object", e )
        {
            @Override
            public Thunk _apply( Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                try
                {
                    SchemeObject so = Scut.as(
                            SchemeObject.class,
                            args.listRef( 0 ) );

                    int i;

                    Method[] methods =
                            so._classAdapter.getMethods();
                    FirstClassObject[] methodsV =
                            new FirstClassObject[ methods.length ];
                    for ( i = 0 ; i < methods.length ; i++ )
                        methodsV[i] = SchemeString.make( methods[i].toString() );

                    Field[] attributes = so._classAdapter.getFields();
                    FirstClassObject[] fieldsV = new FirstClassObject[ attributes.length ];
                    for ( i = 0 ; i < attributes.length ; i++ )
                        fieldsV[i] = SchemeString.make( attributes[i].toString() );

                    var result =
                            new Cons( new Vector( methodsV, false ),
                                    new Vector( fieldsV, false ) );

                    return c.accept( result );
                }
                catch ( ClassCastException e )
                {
                    throw RuntimeX.mInternalError();
                }
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
        tle.setPrimitive( describeObjectProcedure( tle ) );
        tle.setPrimitive( scream_java_make_class( tle ) );
        tle.setPrimitive( scream_java_to_fco( tle ) );
        return tle;
    }
}
