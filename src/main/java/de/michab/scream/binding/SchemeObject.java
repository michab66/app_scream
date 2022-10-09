/* $Id: SchemeObject.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream / JavaBinding
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2003 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.logging.Level;
import java.util.logging.Logger;

import de.michab.scream.Cons;
import de.michab.scream.ConversionFailedX;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.Procedure;
import de.michab.scream.RuntimeX;
import de.michab.scream.SchemeBoolean;
import de.michab.scream.SchemeCharacter;
import de.michab.scream.SchemeDouble;
import de.michab.scream.SchemeInteger;
import de.michab.scream.SchemeString;
import de.michab.scream.Symbol;
import de.michab.scream.Syntax;
import de.michab.scream.Vector;



/**
 * <p>An instance of this class boxes an entity from the Java object system,
 * e.g. an object or a class, i.e. it wraps the objects and implements the
 * Scheme/Java call mapping in both directions.</p>
 * <p>Note that a <code>SchemeObject</code> is basically a
 * <code>FirstClassObject</code>.  It's not yet clear what this really
 * means because it's the result of a very fuzzy thought.</p>
 * <p>But why is it not possible to say something like</p>
 * <pre><code>
 *  (define i (vector 1 2 3 4))
 *  (i (getElement 3))
 * </code></pre>
 * <p>This would allow to implement most of the standard procedures in Scheme.
 * On a more philosophic level it would unify the procedural world of Scheme
 * and the OO world of Java which is definitely good.</p>
 * <p>The only drawback would be that the resulting code for the standard
 * procedures that are used everywhere in Scheme programs would be very
 * reflective and as such not very fast.</p>
 *
 * @author Michael G. Binz
 */
public class SchemeObject
extends Syntax
{
    /**
     * The logger for this class.
     */
    private final static Logger _log =
            Logger.getLogger( SchemeObject.class.getName() );



    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#getTypename()
     */
    public static final String TYPE_NAME = "object";



    /**
     * The instance that is managed.  Note that this must never be null.
     */
    private final java.lang.Object _theInstance;



    /**
     * <code>True</code> if this object represents a Java class,
     * <code>false</code> otherwise.
     */
    private final boolean _isClass;



    /**
     * The instance's class adapter.
     */
    private final JavaClassAdapter _classAdapter;



    /**
     * <p>Create a new SchemeObject for a given object.  The object is used as is,
     * i.e. it isn't copied.  In case the passed object instance is null the
     * instance will be set to the class adapter's embedded class.</p>
     * <p>The resulting SchemeObject then represents a class.</p>
     *
     * @param object The object to be wrapped by the new instance.
     * @param adapter The class adapter for the new instance.
     */
    private SchemeObject( java.lang.Object object, JavaClassAdapter adapter )
    {
        // Create operations with the symbolic name according to the implementing
        // class.
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
     * <code>null</code> is passed for ctor args, the resulting object represents
     * a class and the instance is set to the associated java.lang.Class
     * instance.
     *
     * @param className The name of the class used to create an instance for.
     * @param ctorArgs The list of parameters for the constructor.
     * @return The newly created FirstClasObject.
     * @throws RuntimeX In case of reflection errors.
     */
    private static FirstClassObject createObject(
            String className,
            FirstClassObject[] ctorArgs )
                    throws
                    RuntimeX
    {
        JavaClassAdapter classAdapter = null;

        try
        {
            Class<?> clazz = Class.forName( className );
            // Get the associated class adapter.
            classAdapter = JavaClassAdapter.createObject( clazz );

            // If we didn't receive any constructor arguments...
            if ( null == ctorArgs )
                // ...we create a class representative.
                return new SchemeObject( null, classAdapter );

            // We received constructor arguments, so get the preselected list of
            // available constructors...
            Constructor<?>[] ctors = classAdapter.getConstructors();
            // ...and select the one to call.
            for ( int i = 0 ; i < ctors.length ; i++ )
            {
                java.lang.Object[] argumentList
                = matchParameters( ctors[i].getParameterTypes(), ctorArgs );

                if ( null != argumentList )
                    return convertJava2Scream( ctors[i].newInstance( argumentList ) );
            }

            // No constructor fit the argument list.
            throw new RuntimeX( "METHOD_NOT_FOUND" +
                    new Object[]{ className + "<init>",
                            Cons.create( ctorArgs ) } );
        }
        catch ( ClassNotFoundException e )
        {
            throw new RuntimeX( "CLASS_NOT_FOUND", new Object[]{ className } );
        }
        // We have to catch an error here, since this is thrown if we try to create
        // classes where the case of the class name differs from the actual upper
        // lower case writing. An example for the error message is:
        // "de/michab/scream/JavaCLassAdapter (wrong name:
        // de/michab/scream/JavaClassAdapter)"
        catch ( NoClassDefFoundError e )
        {
            throw new RuntimeX( "CLASS_NOT_FOUND", new Object[]{ className } );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, className );
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

            throw new RuntimeX( "CREATION_FAILED",
                    new Object[]{ what + " " + className } );
        }
        catch ( IllegalAccessException e )
        {
            throw new RuntimeX( "ILLEGAL_ACCESS",
                    new Object[]{ className + "<init>" } );
        }
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



    /**
     * @param context The environment used for necessary evaluations.
     * @param args The passed arguments.
     *
     * @return The operation's result.
     * @throws RuntimeX In case of reflection errors.
     * @see de.michab.scream.Operation#activate(Environment, Cons)
     */
    @Override
    public FirstClassObject activate( Environment context, FirstClassObject[] args )
            throws RuntimeX
    {
        checkMinimumArgumentCount( 1, args );
        checkMaximumArgumentCount( 2, args );

        if ( args.length == 1 )
        {
            if ( args[0] instanceof Cons )
                return processInvocation( context,
                        ((Cons)args[0]).asArray() );

            return processAttributeGet( args[0] );
        }
        else if ( args.length == 2 )
            return processAttributeSet( args[0],
                    // TODO Is that NIL save??
                    evaluate( args[1], context ) );

        throw new RuntimeX( "INTERNAL_ERROR", new Object[]{ SchemeObject.class } );
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
     * Converts an FirstClassObject into an object suitable for inserting into an
     * argument list for a java method call.
     * Currently not all possible conversion are done.  It would be no problem to
     * change this method so it accepts a int scheme argument and converts this
     * to a Java double.  This wasn't done because then we need another order of
     * methods in JavaClassAdapter.  In JDK1.2 the method double abs( double ) is
     * contained in the method list of java.lang.Math long abs( long ).  As a
     * result the integer argument <i>always</i> was converted and there was no
     * possibility to call the integer method.
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

            // First we check the special case:  A NIL is converted to java 'null'.
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
                result = Double.valueOf( ((SchemeDouble)actual).asDouble() );

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
                        (de.michab.scream.Vector)actual );
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
     * Converts a vector, something like <code>#(1 2 3 4)</code>, into a Java
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
            de.michab.scream.Vector actual )
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
     * <code>InvocationTargetException</code>s.  Basically this means that in
     * case the exception embedded in an <code>InvocationTargetException</code>
     * is a <code>RuntimeX</code> then this method unpacks and returns this.  In
     * the other case a new <code>RuntimeX</code> is created and returned.
     * Embedded <code>Error</code>s are unpacked and simply thrown.
     *
     * @param ite The exception to filter.
     * @param context A string to be used in case the embedded exception is not
     *        an <code>Error</code> or <code>RuntimeX</code> and a generic
     *        exception has to be created.
     * @return An instance of a <code>RuntimeX</code> exception.
     * @throws Error Embedded <code>Error</code> instances are thrown.
     */
    public static RuntimeX filterException( InvocationTargetException ite,
            String context )
    {
        Throwable t = ite.getCause();

        _log.log( Level.FINE, t.getMessage(), t );

        // This is needed for handling of the Resync error.  See the description
        // and definition in FirstClassObject.  In case this was no Resync the
        // error will be handled latest in the REP loop.  Note that the Resync
        // error is private to the FirstClassObject implementation and that this
        // must not change.
        if ( t instanceof Error )
            throw (Error)t;

        RuntimeX resultException = null;

        try
        {
            // Try to unbox the exception i.e. if the exception wrapped in the
            // invocation target exception is a RuntimeX then just remove the
            // wrapper.
            resultException = (RuntimeX)t;
        }
        catch ( Exception ee )
        {
            resultException =
                    new RuntimeX( "INVOCATION_EXCEPTION",
                            new Object[]{ context, t } );
        }

        return resultException;
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
        else if ( object.getClass().isArray() )
            return convertJavaArray2ScreamVector( object );

        // Now we test for all primitive types supported by Java.
        else if ( object instanceof java.lang.Integer ||
                object instanceof java.lang.Byte ||
                object instanceof java.lang.Short ||
                object instanceof java.lang.Long )
            return SchemeInteger.createObject( ((java.lang.Number)object).longValue() );

        else if ( object instanceof java.lang.Double ||
                object instanceof java.lang.Float )
            return SchemeDouble.createObject( ((java.lang.Number)object).doubleValue() );

        else if ( object instanceof java.lang.Character )
            return SchemeCharacter.createObject( ((java.lang.Character)object).charValue() );

        else if ( object instanceof String )
            return new SchemeString( (String)object );

        else if ( object instanceof java.lang.Boolean )
            return SchemeBoolean.createObject( ((java.lang.Boolean)object).booleanValue() );

        // This is needed for tightly integrated classes that know about Scream's
        // internal type system.
        else if ( object instanceof FirstClassObject )
            return (FirstClassObject)object;

        // Everything else must be a Java native reference type...
        else
            // ...wrap it and return it.
            return new SchemeObject( object,
                    JavaClassAdapter.createObject( object.getClass()
                            ) );
    }



    /**
     * Converts a Java array into a scream vector.  The passed object must be an
     * array, i.e. <code>Array.isArray()</code> must return true for it.  This is
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
     * @param list The areguments used for the invocation.
     * @return The result of the procedure invocation.
     * @throws RuntimeX In case there where access errors.
     */
    private FirstClassObject processInvocation( Environment env,
            FirstClassObject[] list )
                    throws RuntimeX
    {
        // Check if the first element in the list is a symbol.
        Operation.checkArgument( 1, Symbol.class, list[0] );

        // Evaluate the list, excluding the first entry.
        // TODO this is our callers job.  The environment is only needed because of
        // this loop.
        for ( int i = 1 ; i < list.length ; i++ )
            list[i] = evaluate( list[i], env );

        FirstClassObject[] actual = new FirstClassObject[ list.length -1 ];
        System.arraycopy( list, 1, actual, 0, actual.length );

        java.lang.String methodName = list[0].toString();
        java.lang.reflect.Method[] methods = _classAdapter.getMethods();

        try
        {
            // Select the method to call.
            for ( int i = 0 ; i < methods.length ; i++ )
            {
                // First check the name.
                if ( methodName.equals( methods[i].getName() ) )
                {
                    java.lang.Object[] argumentList
                    = matchParameters( methods[i].getParameterTypes(), actual );

                    if ( null != argumentList )
                    {
                        // Check if it is tried to invoke an instance method on a class
                        // object.  Since the VM in this case simply throws a NPE we have
                        // to check for this condition manually.
                        if ( _isClass && ! Modifier.isStatic( methods[i].getModifiers() ) )
                            throw new RuntimeX( "CANT_ACCESS_INSTANCE" );

                        // Do the actual call.
                        return convertJava2Scream(
                                methods[i].invoke( _theInstance, argumentList ) );
                    }
                }
            }

            throw new RuntimeX( "METHOD_NOT_FOUND",
                    new Object[]{ Cons.create( list ) } );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, methodName );
        }
        catch ( IllegalArgumentException e )
        {
            // Not sure if this can be thrown under normal circumstances.  The only
            // reason know to me for that exception is if it is tried to invoke an
            // instance method on a java.lang.Class object.  In that case the illegal
            // argument is the first argument to invoke, that is on the one hand non
            // null, but on the other hand simply the wrong reference.
            throw new RuntimeX( "ILLEGAL_ARGUMENT",
                    new Object[]{ methodName } );
        }
        catch ( IllegalAccessException e )
        {
            throw new RuntimeX( "ILLEGAL_ACCESS", new Object[]{ methodName } );
        }
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
    private FirstClassObject processAttributeGet( FirstClassObject attribute )
            throws RuntimeX
    {
        // Make sure that the attribute is specified by a Symbol.
        Operation.checkArgument( 1, Symbol.class, attribute );

        try
        {
            // Get the attribute.
            return convertJava2Scream(
                    _classAdapter.getField( attribute.toString() ).get( _theInstance ) );
        }
        catch ( IllegalAccessException e )
        {
            throw new RuntimeX( "ILLEGAL_ACCESS", new Object[]{ attribute } );
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
    private FirstClassObject processAttributeSet( FirstClassObject attribute,
            FirstClassObject value )
                    throws RuntimeX
    {
        // Make sure that the attribute is specified by a Symbol.
        Operation.checkArgument( 1, Symbol.class, attribute );

        try
        {
            // Now try to get a reference to the field...
            Field field = _classAdapter.getField( attribute.toString() );

            // ...and prepare the value to be set in the field.  If this fails we
            // will end up in the exception handler below.
            Object[] newValue =
                    new Object[]{ convertScream2Java( field.getType(), value ) };

            // Everything fine so far, now we set the value.  If we are not
            // allowed to do this the exception handler will help us out.
            field.set( _theInstance, newValue[0] );

            // Since we are free to specify what to return from this method, we
            // return the value we set on the field.
            return value;
            // An alternative could be to read the field, re-convert the received
            // object into the Scream type system and return this.  But this isn't
            // really cheap and not really needed so we decided against it.
        }
        catch ( IllegalAccessException e )
        {
            throw new RuntimeX( "ILLEGAL_ACCESS", new Object[]{ attribute } );
        }
        catch ( ConversionFailedX e )
        {
            throw new RuntimeX( e.getMessage() );
        }
    }



    /**
     * Test for equality based on the embedded instance.  If we have no instance
     * which is the case for class representatives test if the same
     * <code>JavaClassAdapter</code> is used.
     *
     * @param other The object to compare.
     * @return <code>True</code> if the reference passed into this call is eqv to
     *         this object.
     */
    @Override
    public boolean eqv( FirstClassObject other )
    {
        try
        {
            // Implicit type checking.  Won't survive the cast if this is no
            // SchemeObject.
            SchemeObject otherSo = (SchemeObject)other;

            return _theInstance.equals( otherSo._theInstance );
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }



    /**
     * Create a string from this object.  This can't be used to read the object
     * back in.
     *
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
    public Object clone()
    {
        try
        {
            // Get the clone method...
            Method cloneMethod =
                    _theInstance.getClass().getMethod( "clone", new Class[0] );
            // ...and try to call it.  Since this is protected on java.lang.Object,
            // we try it via reflection.
            return new SchemeObject(
                    cloneMethod.invoke( _theInstance, new Object[0] ) );
        }
        catch ( Exception e )
        {
            // Well, cloning failed so return identity.
            return this;
        }
    }



    // Procedure definitions.



    /**
     * (make-object createlist) where createlist is (string:classname ctorarg1 ...)
     */
    static private Syntax constructObjectSyntax = new Syntax( "make-object" )
    {
        @Override
        public FirstClassObject activate( Environment context, FirstClassObject[] args )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            if ( (args[0] instanceof Cons) && ((Cons)args[0]).isProperList() )
            {
                FirstClassObject[] createList = ((Cons)args[0]).asArray();

                // Test if first list element is symbol.
                checkArgument( 1, Symbol.class, createList[0] );

                // Now evaluate the list of constructor arguments, but not the first
                // list element.
                for ( int i = 1 ; i < createList.length ; i++ )
                    createList[i] = evaluate( createList[i], context );

                FirstClassObject[] params = new FirstClassObject[ createList.length -1 ];
                System.arraycopy( createList, 1, params, 0, params.length );
                return createObject( createList[0].toString(), params );
            }
            else if ( args[0] instanceof Symbol )
            {
                // Create a sole class representative.  This object is only of use to
                // access class (static) fields or methods.
                return createObject( args[0].toString(), null );
            }
            else
                throw new RuntimeX( "SYNTAX_ERROR" );
        }
    };



    /**
     * (object obj) -- Wraps the passed first class object with a scheme object.
     */
    static private Procedure wrapObjectProcedure = new Procedure( "object" )
    {
        @Override
        public FirstClassObject apply( FirstClassObject[] args )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            FirstClassObject result;

            // If the passed object is a SchemeObject...
            if ( args[0] == Cons.NIL || args[0] instanceof SchemeObject )
                // ...then just return this one.
                result = args[0];

            else
                // In the other case return a wrapped object.
                result = new SchemeObject( args[0] );

            return result;
        }
    };




    /**
     * (object? obj)
     */
    static private Procedure objectPredicateProcedure = new Procedure( "object?" )
    {
        @Override
        public FirstClassObject apply( FirstClassObject[] args )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );
            return SchemeBoolean.createObject( args[0] instanceof SchemeObject );
        }
    };



    /**
     * (describe-object obj) -> #f
     *
     * TODO: return a more scheme like representation...
     */
    static private Procedure describeObjectProcedure =
            new Procedure( "describe-object" )
    {
        @Override
        public FirstClassObject apply( FirstClassObject[] args )
                throws RuntimeX
        {
            checkArgumentCount( 1, args );

            try
            {
                // This cast also performs implicit type checking.  If this is no
                // SchemeObject we will continue in the catch.
                SchemeObject so = (SchemeObject)args[0];

                int i;

                Method[] methods = so._classAdapter.getMethods();
                FirstClassObject[] methodsV = new FirstClassObject[ methods.length ];
                for ( i = 0 ; i < methods.length ; i++ )
                    methodsV[i] = new SchemeString( methods[i].toString() );

                Field[] attributes = so._classAdapter.getFields();
                FirstClassObject[] fieldsV = new FirstClassObject[ attributes.length ];
                for ( i = 0 ; i < attributes.length ; i++ )
                    fieldsV[i] = new SchemeString( attributes[i].toString() );

                return
                        new Cons( new Vector( methodsV, false ),
                                new Vector( fieldsV, false ) );
            }
            catch ( ClassCastException e )
            {
                return Cons.NIL;
            }
        }
    };



    /**
     * <p><code>(%catch expression error-handler)</code></p>
     * Evaluates the passed <code>expression</code> and executes the
     * <code>error-handler</code> as soon as an error occurs in exception
     * execution.  The error handler is executed for its side effects, <i>the
     * error ultimately submitted is the original error</i>.  Errors inside the
     * error-handler override the original error.
     */
    static private Syntax catchExceptionSyntax =
            new Syntax( "%catch" )
    {
        @Override
        public FirstClassObject activate( Environment context,
                FirstClassObject[] args )
                        throws RuntimeX
        {
            checkArgumentCount( 2, args );

            try
            {
                return FirstClassObject.evaluate( args[0], context );
            }
            catch ( RuntimeX e )
            {
                // Execute the error-handler for its side effects...
                FirstClassObject.evaluate( args[1], context );
                // ...then re-throw the original exception.  If an error happened in
                // the above invocation of the error-handler, then this is thrown
                // instead.
                throw e;
            }
        }
    };



    /**
     * Object operations setup.
     *
     * @param tle The environment to extend with additional operations.
     * @return The extended environment.  This is identical to the enivonment
     *         passed in.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    // Note that it is *not* expected that this method overrides the method in
    // the super class.  Both methods are called by the initialisation logic.
    {
        tle.setPrimitive( objectPredicateProcedure );
        tle.setPrimitive( wrapObjectProcedure );
        tle.setPrimitive( constructObjectSyntax );
        tle.setPrimitive( describeObjectProcedure );
        tle.setPrimitive( catchExceptionSyntax );
        return tle;
    }
}
