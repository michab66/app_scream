/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Objects;
import java.util.logging.Logger;

import org.smack.util.JavaUtil;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeBoolean;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.fcos.SchemeInteger;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Syntax;
import de.michab.scream.fcos.Vector;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.ConversionFailedX;
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
    /**
     * The logger for this class.
     */
    private final static Logger LOG =
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
    private final java.lang.Object _theInstance;

    /**
     * {@code True} if this object represents a Java class,
     * {@code false} otherwise.
     */
    private final boolean _isClass;

    /**
     * The instance's class adapter.
     */
    private final JavaClassAdapter _classAdapter;

    /**
     * Create a new SchemeObject for a given object.  The object is used as is,
     * i.e. it isn't copied.  In case the passed object instance is null the
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

        if ( object == null )
        {
            _isClass = true;
            _theInstance = adapter.adapterFor();
        }
        else
        {
            _isClass = false;
            _theInstance = object;
        }

        _classAdapter = adapter;
    }

    /**
     * Create a new SchemeObject for a given object.  The object is used as is,
     * i.e. it isn't copied.
     *
     * @param object The object to be wrapped by the new instance.
     */
    public SchemeObject( java.lang.Object object )
    {
        this( object, JavaClassAdapter.createObject( object.getClass() ) );
    }

    /**
     * The factory for SchemeObjects.  Note that if a FirstClassObject is created
     * this is not wrapped with a SchemeObject but returned as is.  If
     * {@code null} is passed for ctor args, the resulting object represents
     * a class and the instance is set to the associated java.lang.Class
     * instance.
     *
     * @param className The name of the class used to create an instance for.
     * @param ctorArgs The list of parameters for the constructor.
     * @param c The continuation that receives the new object.
     * @return The thunk.
     * @throws RuntimeX In case of reflection errors.
     */
    private static Thunk createObject(
            String className,
            FirstClassObject[] ctorArgs,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        JavaClassAdapter classAdapter = null;

        // Used to transfer the constructor into the exception handlers.
        Executable constructor = null;

        try
        {
            // Get the associated class adapter.
            classAdapter = JavaClassAdapter.createObject(
                    Class.forName( className ) );

            // If we didn't receive any constructor arguments...
            if ( null == ctorArgs )
                // ...we create a class representative.
                return c.accept( new SchemeObject( null, classAdapter ) );

            // We received constructor arguments, select the ctor to call.
            for ( var ctor : classAdapter.getConstructors() )
            {
                var argumentList =
                        matchParameters( ctor.getParameterTypes(), ctorArgs );

                if ( null == argumentList )
                    continue;

                constructor = Objects.requireNonNull( ctor );

                return c.accept( convertJava2Scream( ctor.newInstance( argumentList ) ) );
            }

            // No constructor fit the argument list.
            throw RuntimeX.mMethodNotFound(
                    className + "<init>" + Arrays.toString( ctorArgs ));
        }
        catch ( ClassNotFoundException e )
        {
            throw RuntimeX.mClassNotFound( className );
        }
        // We have to catch an error here, since this is thrown if we try to create
        // classes where the case of the class name differs from the actual upper
        // lower case writing. An example for the error message is:
        // "de/michab/scream/JavaCLassAdapter (wrong name:
        // de/michab/scream/JavaClassAdapter)"
        catch ( NoClassDefFoundError e )
        {
            throw RuntimeX.mClassNotFound( className );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, constructor );
        }
        catch ( InstantiationException e )
        {
            int modifiers = classAdapter.adapterFor().getModifiers();
            String what;

            if ( Modifier.isAbstract( modifiers ) )
                what = "abstract class";
            else if ( Modifier.isInterface( modifiers ) )
                what = "interface";
            else
                what = "";

            throw RuntimeX.mCreationFailed(
                    what + " " + className );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess(
                    className + "<init>"  );
        }
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

    /**
     * Return the Java object that corresponds to the Scheme object.
     * @return The corresponding Java object.
     */
    @Override
    public Object toJava()
    {
        return _theInstance;
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        long argsLen =
                checkArgumentCount( 1, 2, args );

        var args0 = args.listRef( 0 );

        if ( argsLen == 1 && args0 instanceof Cons )
        {
            return processInvocation(
                    e,
                    (Cons)args0,
                    c );
        }

        if ( argsLen == 1 && args0 instanceof Symbol)
        {
            return processAttributeGet(
                    (Symbol)args0,
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
     * Matches the actual parameter list of the invocation with the passed
     * formal parameter list.
     *
     * @param formal An array of classes representing the formal parameter list.
     * @param actual An array of objects representing the actual parameters.
     * @return An array of objects converted from the scream types to java types
     *         suitable for invoking the method.  If no match was possible null
     *         is returned.
     */
    private static Object[] matchParameters(
            Class<?>[] formal,
            FirstClassObject[] actual )
    {
        // Check the length.
        if ( formal.length != actual.length )
            return null;

        try
        {
            Object[] result = new Object[ formal.length ];

            for ( int i = 0 ; i < result.length ; i++ )
                result[i] = convertScream2Java( formal[i], actual[i] );

            return result;
        }
        catch ( RuntimeX e )
        {
            return null;
        }
    }

    /**
     * Converts a FirstClassObject into an object suitable for inserting into an
     * argument list for a java method call.
     * <p>
     * Currently not all possible conversion are done.  It would be no problem to
     * change this method so it accepts a int scheme argument and converts this
     * to a Java double.  This wasn't done because then we need another order of
     * methods in JavaClassAdapter.  In JDK1.2 the method double abs( double ) is
     * contained in the method list of java.lang.Math long abs( long ).  As a
     * result the integer argument <i>always</i> was converted and there was no
     * possibility to call the integer method.
     * <p>
     * A similar example is the trivial boolean conversion that explicitly is
     * <i>not</i> carried out by this method:  In Scheme everything but #F is #T.
     * If we would implement this here, the result were that if by chance the
     * anyType method( boolean ); were the first in the classes method list we
     * never are able to call another one argument method, since all single arg
     * calls were catched by this.
     *
     * @param formal The class expected in the java argument list.
     * @param actual The FirstClassObject to convert.
     * @return An Object resulting from the conversion.  This is ready to be
     *          inserted into the argument list for an invoke() call.
     * @throws ConversionFailedX Is thrown if the conversion failed.
     */
    public static Object convertScream2Java(
            Class<?> formal,
            FirstClassObject actual )
                    throws
                    RuntimeX
    {
        try
        {
            Object result = null;

            // First we check the special case:  A NIL is converted to Java 'null'.
            if ( actual == Cons.NIL )
                result = null;

            // Check for java primitive types
            else if ( formal == java.lang.Boolean.TYPE )
                result = Boolean.valueOf( ((SchemeBoolean)actual).getValue() );

            else if ( formal == java.lang.Byte.TYPE )
                result = Byte.valueOf( (byte)((SchemeInteger)actual).asLong() );

            else if ( formal == java.lang.Short.TYPE )
                result = Short.valueOf( (short)((SchemeInteger)actual).asLong() );

            else if ( formal == java.lang.Integer.TYPE )
                result = Integer.valueOf( (int)((SchemeInteger)actual).asLong() );

            else if ( formal == java.lang.Long.TYPE )
                result = Long.valueOf( ((SchemeInteger)actual).asLong() );

            else if ( formal == java.lang.Float.TYPE )
                result = Float.valueOf( (float)((SchemeDouble)actual).asDouble() );

            else if ( formal == java.lang.Double.TYPE )
                result = Double.valueOf( ((Number)actual).asDouble() );

            else if ( formal == java.lang.Character.TYPE )
                result = Character.valueOf( ((SchemeCharacter)actual).asCharacter() );

            else if ( formal == java.lang.String.class )
                result = ((SchemeString)actual).getValue();

            // This handles the special case that a method is called that knows
            // Scream's internal type system.  Currently this is used by the
            // AwtListener class that directly accepts Cons lists.
            // Holy cow, this is one of the most complex code lines I ever wrote.
            // The condition is only taken if the formal argument is some kind of a
            // FirstClassObject and the passed actual argument can be assigned to
            // that type.
            else if ( FirstClassObject.class.isAssignableFrom( formal ) &&
                    formal.isAssignableFrom( actual.getClass() ) )
                result = actual;

            // Check for and convert arrays.
            else if ( formal.isArray() )
            {
                result = convertScreamVector2JavaArray(
                        formal,
                        (de.michab.scream.fcos.Vector)actual );
            }

            // The last chance conversion.
            else if ( actual instanceof FirstClassObject )
            {
                // According to the new rules in this class the instance must never be
                // null.
                Object actualValue = actual.toJava();

                if ( formal.isAssignableFrom( actualValue.getClass() ) )
                    result = actualValue;
                else
                    throw new ConversionFailedX( actual, formal );
            }

            else
                throw new ConversionFailedX( actual, formal );

            return result;
        }
        catch ( ClassCastException e )
        {
            throw new ConversionFailedX( actual, formal );
        }
    }

    /**
     * Converts a vector, something like {@code #(1 2 3 4)}, into a Java
     * array.
     *
     * @param formal The array type (a.k.a. component type) for the resulting
     *        array.
     * @param actual The scheme vector to convert.
     * @return The resulting array.
     * @throws ConversionFailedX In case there were type errors.
     */
    private static Object convertScreamVector2JavaArray(
            Class<?> formal,
            de.michab.scream.fcos.Vector actual )
                    throws
                    RuntimeX
    {
        var len = actual.size();
        Class<?> componentType = formal.getComponentType();

        // Create the result array.
        Object result = Array.newInstance( componentType, (int)len );

        for ( int i = 0 ; i < len ; i++ )
            Array.set( result, i, convertScream2Java(
                    componentType,
                    actual.get( i ) ) );

        return result;
    }

    /**
     * This method is responsible for handling
     * } {@code InvocationTargetException}s.  Basically this means that in
     * case the exception embedded in an {@code InvocationTargetException}
     * is a {@code RuntimeX} then this method unpacks and returns this.  In
     * the other case a new {@code RuntimeX} is created and returned.
     * Embedded {@code Error}s are unpacked and simply thrown.
     *
     * @param ite The exception to filter.
     * @param context A string to be used in case the embedded exception is not
     *        an {@code Error} or {@code RuntimeX} and a generic
     *        exception has to be created.
     * @return An instance of a {@code RuntimeX} exception.
     * @throws Error Embedded {@code Error} instances are thrown.
     */
    static RuntimeX filterException( InvocationTargetException ite,
            Executable context )
    {
        Throwable t = ite.getCause();
        JavaUtil.Assert( t != null );

        try
        {
            return (RuntimeX)t;
        }
        catch ( Exception ee )
        {
            return RuntimeX.mInvocationException(
                    context,
                    t );
        }
    }

    /**
     * Converts a Java object back into Scream's type system.
     *
     * @param object The object to be boxed.
     * @return The object representing the box.
     */
    public static FirstClassObject convertJava2Scream( java.lang.Object object )
    {
        // Java nulls are mapped into NIL.
        if ( null == object )
            return Cons.NIL;

        // Test for array types...
        if ( object.getClass().isArray() )
            return convertJavaArray2ScreamVector( object );

        // Now we test for all primitive types supported by Java.
        if ( object instanceof java.lang.Integer ||
                object instanceof java.lang.Byte ||
                object instanceof java.lang.Short ||
                object instanceof java.lang.Long )
            return SchemeInteger.createObject( ((java.lang.Number)object).longValue() );

        if ( object instanceof java.lang.Double ||
                object instanceof java.lang.Float )
            return SchemeDouble.createObject( ((java.lang.Number)object).doubleValue() );

        if ( object instanceof java.lang.Character )
            return SchemeCharacter.createObject( ((java.lang.Character)object).charValue() );

        if ( object instanceof String )
            return SchemeString.make( (String)object );

        if ( object instanceof java.lang.Boolean )
            return SchemeBoolean.createObject( ((java.lang.Boolean)object).booleanValue() );

        // This is needed for tightly integrated classes that know about Scream's
        // internal type system.
        if ( object instanceof FirstClassObject )
            return (FirstClassObject)object;

        // Everything else must be a Java-native reference type...
        // ...wrap it and return it.
        return new SchemeObject( object,
                JavaClassAdapter.createObject( object.getClass()
                        ) );
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

    private Thunk processInvocationImpl(
            Environment env,
            String methodName,
            Cons list,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        Executable methodRef = null;

        try
        {
            // Select the method to call.
            for ( var method : _classAdapter.getMethods() )
            {
                // First check the name.
                if ( methodName.equals( method.getName() ) )
                {
                    java.lang.Object[] argumentList =
                            matchParameters( method.getParameterTypes(), Cons.asArray( list ) );

                    if ( null != argumentList )
                    {
                        // Check if it is tried to invoke an instance method on a class
                        // object.  Since the VM in this case simply throws a NPE we have
                        // to check for this condition manually.
                        if ( _isClass && ! Modifier.isStatic( method.getModifiers() ) )
                            throw RuntimeX.mCannotAccessInstance();
                        // Store the method reference for the exception handler.
                        methodRef = method;
                        // Do the actual call.
                        return c.accept( convertJava2Scream(
                                method.invoke( _theInstance, argumentList ) ) );
                    }
                }
            }

            throw RuntimeX.mMethodNotFound( methodName, list );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, methodRef );
        }
        catch ( IllegalArgumentException e )
        {
            // Not sure if this can be thrown under normal circumstances.  The only
            // reason know to me for that exception is if it is tried to invoke an
            // instance method on a java.lang.Class object.  In that case the illegal
            // argument is the first argument to invoke, that is on the one hand non
            // null, but on the other hand simply the wrong reference.
            throw RuntimeX.mIllegalArgument( methodName );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( methodName );
        }
    }
    private Thunk _processInvocationImpl(
            Environment env,
            String methodName,
            Cons list,
            Cont<FirstClassObject> c )
    {
        return () -> processInvocationImpl( env, methodName, list, c );
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
        var symbol = Scut.as(
                Symbol.class, list.getCar() );
        var rest = Scut.as(
                Cons.class, list.getCdr() );

        return Primitives._evalCons(
                env,
                rest,
                evaluated -> _processInvocationImpl(
                        env,
                        symbol.toString(),
                        evaluated,
                        c ) );
    }

    /**
     * Get/Read an attribute from the passed object.
     *
     * @param attribute The attribute to read.  This call is NIL safe, i.e. the
     *        type of the FCO is expected to be Symbol, in all other cases an
     *        exception will be thrown.
     * @return The attribute's value.
     * @throws RuntimeX In case the attribute could not be read.
     */
    private Thunk processAttributeGet( Symbol attribute, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        LOG.info( attribute.toString() );

        try
        {
            // Get the attribute.
            return c.accept( convertJava2Scream(
                    _classAdapter.getField( attribute.toString() ).get( _theInstance ) ) );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( attribute.toString() );
        }
    }

    /**
     * Sets an attribute on the passed object.
     *
     * @param attribute The attribute to set.  Attribute has to be of type
     *        symbol, in all other cases an exception will be thrown.
     * @param value The value to set.  This has to respect the type of the
     *        attribute parameter.
     * @return The attribute's new value.
     * @throws RuntimeX In case the attribute could not be set.
     */
    private Thunk processAttributeSet(
            Symbol attribute,
            FirstClassObject value,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        LOG.info( attribute + " = " + value );

        try
        {
            Field field = _classAdapter.getField(
                    attribute.toString() );

            field.set(
                    _theInstance,
                    convertScream2Java( field.getType(), value ) );

            return c.accept( value );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( attribute.toString() );
        }
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

            return _theInstance.equals( otherSo._theInstance );
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
        return "@Object:" + _theInstance.getClass().getName() + "=" + _theInstance;
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
        protected Thunk __executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 1, args );

            FirstClassObject argument = args.getCar();

            // This creates a class instance. TODO not intuitive.
            if ( argument instanceof Symbol )
                return createObject( argument.toString(), null, c );

            Cons cons = Scut.as(
                    Cons.class,
                    argument );
            Cons arguments = Scut.as(
                    Cons.class,
                    cons.getCdr() );
            Symbol name = Scut.as(
                    Symbol.class,
                    cons.getCar() );

            return Primitives._evalCons(
                    e,
                    arguments,
                    evaluated ->
                            _createObject(
                                    name.toString(),
                                    evaluated,
                                    c) );
        }
    };

    /**
     * (object obj) -- Wraps the passed first class object with a scheme object.
     */
    static private Procedure wrapObjectProcedure( Environment e )
    {
        return new Procedure( "object" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                var a0 = args.getCar();

                // If the passed object is already a SchemeObject...
                if ( a0 == Cons.NIL || a0 instanceof SchemeObject )
                    // ...then just return this one.
                    return c.accept( a0 );

                return c.accept( new SchemeObject( a0 ) );
            }
        }.setClosure( e );
    }

    /**
     * (object? obj)
     */
    static private Procedure objectPredicateProcedure( Environment e )
    {
        return new Procedure( "object?" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return () -> c.accept(
                        SchemeBoolean.createObject( args.getCar() instanceof SchemeObject ) );
            }
        }.setClosure( e );
    }

//    /**
//     * (describe-object obj) -> #f
//     *
//     * TODO: return a more scheme like representation...
//     */
//    static private Procedure describeObjectProcedure =
//            new Procedure( "describe-object" )
//    {
//        @Override
//        public FirstClassObject apply( FirstClassObject[] args )
//                throws RuntimeX
//        {
//            checkArgumentCount( 1, args );
//
//            try
//            {
//                SchemeObject so = (SchemeObject)args[0];
//
//                int i;
//
//                Method[] methods = so._classAdapter.getMethods();
//                FirstClassObject[] methodsV = new FirstClassObject[ methods.length ];
//                for ( i = 0 ; i < methods.length ; i++ )
//                    methodsV[i] = new SchemeString( methods[i].toString() );
//
//                Field[] attributes = so._classAdapter.getFields();
//                FirstClassObject[] fieldsV = new FirstClassObject[ attributes.length ];
//                for ( i = 0 ; i < attributes.length ; i++ )
//                    fieldsV[i] = new SchemeString( attributes[i].toString() );
//
//                return
//                        new Cons( new Vector( methodsV, false ),
//                                new Vector( fieldsV, false ) );
//            }
//            catch ( ClassCastException e )
//            {
//                return Cons.NIL;
//            }
//        }
//    };
//
    /**
     * <p>{@code (%catch expression error-handler)}</p>
     * Evaluates the passed {@code expression} and executes the
     * {@code error-handler} as soon as an error occurs in exception
     * execution.  The error handler is executed for its side effects, <i>the
     * error ultimately submitted is the original error</i>.  Errors inside the
     * error-handler override the original error.
     */
//    static private Syntax catchExceptionSyntax =
//            new Syntax( "%catch" )
//    {
//        @Override
//        public FirstClassObject activate( Environment context,
//                FirstClassObject[] args )
//                        throws RuntimeX
//        {
//            checkArgumentCount( 2, args );
//
//            try
//            {
//                return FirstClassObject.evaluate( args[0], context );
//            }
//            catch ( RuntimeX e )
//            {
//                // Execute the error-handler for its side effects...
//                FirstClassObject.evaluate( args[1], context );
//                // ...then re-throw the original exception.  If an error happened in
//                // the above invocation of the error-handler, then this is thrown
//                // instead.
//                throw e;
//            }
//        }
//    };

    /**
     * Object operations setup.
     *
     * @param tle The environment to extend with additional operations.
     * @return The extended environment.  This is identical to the enivonment
     *         passed in.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( objectPredicateProcedure(tle) );
        tle.setPrimitive( wrapObjectProcedure( tle ) );
        tle.setPrimitive( constructObjectSyntax );
//        tle.setPrimitive( describeObjectProcedure );
//        tle.setPrimitive( catchExceptionSyntax );
        return tle;
    }
}
