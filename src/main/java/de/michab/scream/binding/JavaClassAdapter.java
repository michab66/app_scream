/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.Objects;
import java.util.logging.Logger;

import org.smack.util.JavaUtil;
import org.smack.util.Pair;
import org.smack.util.ReflectionUtil;
import org.smack.util.StringUtil;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Vector;
import de.michab.scream.util.MapWithProducerX;
import de.michab.scream.util.Scut;

/**
 * Encapsulates information for an arbitrary Java class.
 *
 * @author Michael Binz
 */
public final class JavaClassAdapter
{
    /**
     * The logger for this class.
     */
    @SuppressWarnings("unused")
    private final static Logger LOG =
            Logger.getLogger( JavaClassAdapter.class.getName() );

    private final static MapWithProducerX<Class<?>, JavaClassAdapter, RuntimeException>
    _classAdapterCache =
            new MapWithProducerX<>( JavaClassAdapter::new );

    /**
     * The {@code java.lang.Class} this object is associated with.
     */
    private final Class<?> _clazz;

    /**
     * Constructs a class adapter for the passed class.
     *
     * @param cl The class to create an adapter for.
     */
    private JavaClassAdapter( Class<?> cl )
    {
        _clazz = cl;
    }

    /**
     * Get a class adapter instance.
     *
     * @param cl The class to create an adapter for.
     * @return A class adapter for the passed class.
     */
    public static JavaClassAdapter get( Class<?> cl )
    {
        return _classAdapterCache.get(
                Objects.requireNonNull( cl ) );
    }

    /**
     * Get a class adapter instance.
     *
     * @param classname The name of the class.
     * @return A class adapter for the passed class.
     */
    public static JavaClassAdapter get( String classname )
        throws RuntimeX
    {
        try
        {
            return get( Class.forName( classname ) );
        }
        catch ( ClassNotFoundException e )
        {
            throw RuntimeX.mClassNotFound( classname );
        }
    }

    /**
     * A map holding already resolved methods.
     */
    private final MapWithProducerX<String, Method, RuntimeX> _methodCache =
            new MapWithProducerX<>( this::getMethodImpl );

    private Method getMethodImpl( String name )
            throws RuntimeX
    {
        var nameArguments = JavaClassAdapter.makeNameArguments( name );

        String braced = nameArguments.right;

        for ( var c : _clazz.getMethods() )
        {
            if ( ! c.getName().equals( nameArguments.left ) )
                continue;

            if ( ! c.toString().contains( braced ) )
                continue;

            if ( c.isSynthetic() )
                continue;

            return c;
        }

        throw RuntimeX.mMethodNotFound(
                _clazz.getSimpleName() + "#" + nameArguments.left + braced );
    }

    public Method getMethod( String name  )
            throws RuntimeX
    {
        return _methodCache.get( name );
    }

   /**
    * A map holding already resolved constructors.
    */
    private static final MapWithProducerX<String,Constructor<?>,RuntimeX> _ctorCache =
            new MapWithProducerX<>( JavaClassAdapter::getCtorImpl );

    private static Constructor<?> getCtorImpl( String name )
            throws RuntimeX
    {
        var nameArguments = JavaClassAdapter.makeNameArguments( name );

        var cl = get( nameArguments.left );

        for ( var c : cl._clazz.getConstructors() )
        {
            if ( ! c.getName().equals( nameArguments.left ) )
                continue;

            if ( ! c.toString().contains( nameArguments.right ) )
                continue;

            if ( c.isSynthetic() )
                continue;

            return c;
        }

        throw RuntimeX.mMethodNotFound(
                nameArguments.left + "<init>" + nameArguments.right );
    }

    /**
     * Get a named constructor.
     *
     * @param name Name in format "classname:ctor-arg-type,...".
     * @return A constructor.
     *
     * @throws RuntimeX
     * @see {@link #makeNameArguments(String)}
     */
    public static Constructor<?> getCtor( String name )
            throws RuntimeX
    {
        return _ctorCache.get( name );
    }

    /**
     * Dynamically create a class implementing all the passed interfaces.
     *
     * @param interfaces A list of the interfaces the returned class adapter
     *        has to support.
     * @return A class adapter for the passed interfaces.
     */
//    public static JavaClassAdapter createObject( Class<?>[] interfaces )
//    {
//        // Create an unique name from the list of passed interfaces.  This will be
//        // used for hashing the created class.
//        StringBuffer tmpName = new StringBuffer();
//        for ( int i = 0 ; i < interfaces.length ; i++ )
//            tmpName.append( interfaces[i].getName() );
//
//        String name = tmpName.toString();
//
//        // Check if a class adapter exists for this class.
//        JavaClassAdapter result =
//                _classAdapterInstances.get( name );
//
//        if ( result != null )
//            return result;
//
//        // No class adapter did exist, so let's build one.  As a first step add
//        // another interface to the list of passed interfaces...
//        Class<?>[] extendedItfList = new Class[ interfaces.length +1 ];
//        System.arraycopy( interfaces, 0, extendedItfList, 0, interfaces.length );
//        interfaces = extendedItfList;
//        interfaces[ interfaces.length-1 ] = InterfaceConfigurator.class;
//
//        // ...create the class...
//        Class<?> clazz = Proxy.getProxyClass( JavaClassAdapter.class.getClassLoader(),
//                interfaces );
//        result = new JavaClassAdapter( clazz );
//        // ...save it in the hashtable...
//        _classAdapterInstances.put( name, result );
//        // ...and return it.
//        return result;
//    }
//
    /**
     * Create an instance for a dynamically created interface.  The class adapter
     * has to be the result of a call to the array based createObject() method.
     *
     * @return The instantiated object.
     * @throws RuntimeX If the instantiation failed.
     */
    public Object instantiateInterface()
            throws RuntimeX
    {
        if ( ! Proxy.isProxyClass( _clazz ) )
            throw RuntimeX.mNoProxy( _clazz.getName() );

        Constructor<?> c = null;
        try
        {
            c = _clazz.getConstructor( new Class[]{ InvocationHandler.class } );

            return c.newInstance( new Object[] { new InterfaceInvocationHandler() });
        }
        catch( InvocationTargetException e )
        {
            throw filterException( e, c );
        }
        catch ( NoSuchMethodException e )
        {
            throw RuntimeX.mProxyCannotInstantiate( _clazz.getName() );
        }
        catch ( IllegalAccessException e )
        {
            throw RuntimeX.mIllegalAccess( _clazz.getName() );
        }
        catch ( InstantiationException e )
        {
            throw RuntimeX.mProxyCannotInstantiate( _clazz.getName() );
        }
    }

    /**
     * Returns the class this adapter adapts to.
     *
     * @return The class this adapter adapts to.
     */
    public Class<?> adapterFor()
    {
        return _clazz;
    }

    public  Method[] getMethods()
    {
        return _clazz.getDeclaredMethods();
    }

    /**
     * @return The filtered set of fields accessible from Scream.
     */
    public Field[] getFields()
    {
        return _clazz.getFields();
    }

    /**
     * Get a reference to a field in this class.
     *
     * @param name The field's name.
     * @return A reference to the field.
     * @throws RuntimeX In case the field doesn't exist.
     */
    public Field getField( String name )
            throws RuntimeX
    {
        try
        {
            return _clazz.getField( name );
        }
        catch ( NoSuchFieldException e )
        {
            throw RuntimeX.mFieldNotFound( name );
        }
    }

    /**
     * Return a string representation of this object.
     *
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return _clazz.toString();
    }

    private static Object mapArray( Class<?> componentType, FirstClassObject[] fcos )
            throws RuntimeX
    {
        Object result = Array.newInstance(
                componentType,
                fcos.length );

        for ( int i = 0 ; i < fcos.length ; i++ )
            Array.set(
                    result,
                    i,
                    map(
                    fcos[i],
                    componentType ) );

        return result;
    }

    /**
     * Maps a vector to a Java array.
     *
     * @param componentType The component type) of the result array.
     * @param vector The Scheme vector to convert.
     * @return The result array.
     * @throws RuntimeX In case of type errors.
     */
    private static Object mapArray( Class<?> componentType, Vector vector )
            throws RuntimeX
    {
        return mapArray( componentType, vector.asArray() );
    }

    /**
     * Maps a list to a Java array of a given component type.
     *
     * @param componentType The component type of the result array.
     * @param list A proper list of fcos.
     */
    private static Object mapArray( Class<?> componentType, Cons list )
            throws RuntimeX
    {
        if ( ! Cons.isProper( list ) )
            throw RuntimeX.mExpectedProperList( list );

        return mapArray( componentType, Cons.asArray( list ) );
    }

    /**
     *
     * @param fco
     * @param cl
     * @return
     * @throws RuntimeX
     */
    private static Object map( FirstClassObject fco, Class<?> cl )
            throws RuntimeX
    {
        cl = ReflectionUtil.normalizePrimitives( cl );

        if ( FirstClassObject.class.isAssignableFrom( cl ) )
            return fco;

        // Map arrays.
        if ( cl.isArray() && FirstClassObject.is( Vector.class, fco ) )
            return mapArray( cl.getComponentType(), Scut.as( Vector.class, fco ) );
        if ( cl.isArray() && FirstClassObject.is( Cons.class, fco ) )
            return mapArray( cl.getComponentType(), Scut.as( Cons.class, fco ) );
        if ( cl.isArray() )
            throw RuntimeX.mTypeError( Vector.class, fco );

        // Handle SchemeObjects.
        if ( fco instanceof SchemeObject )
            return fco.toJava();

        // Convert primitives.
        if ( cl == Boolean.class  )
            return Boolean.valueOf( fco == Bool.F ? Boolean.FALSE : Boolean.TRUE );
        if ( cl == Byte.class )
            return Byte.valueOf( assertByte( Scut.as( Int.class, fco ) ) );
        if ( cl == Short.class )
            return Short.valueOf( assertShort( Scut.as( Int.class, fco ) ) );
        if ( cl == Integer.class )
            return Integer.valueOf( assertInt( Scut.as( Int.class, fco ) ) );
        if ( cl == Long.class )
            return Long.valueOf( Scut.as( Number.class, fco ).asLong() );
        if ( cl == Character.class )
            return Character.valueOf( Scut.as( SchemeCharacter.class, fco ).asCharacter() );
        if ( cl == Float.class )
            return Float.valueOf( (float)Scut.as( Number.class, fco ).asDouble() );
        if ( cl == Double.class )
            return Double.valueOf( Scut.as( Number.class, fco ).asDouble() );
        if ( cl == String.class && fco instanceof SchemeString )
            return fco.toJava();

        // Last order: Perform a string conversion.
        if ( cl == String.class  )
            return FirstClassObject.toString( fco );

        if ( cl == Object.class )
            return fco.toJava();

        // TODO cannot express the target type.
        throw RuntimeX.mTypeError( SchemeObject.class, fco );
    }

    public static Object createInstanceVariadic(
            Constructor<?> method,
            FirstClassObject[] args ) throws RuntimeX
    {
        if ( args.length < method.getParameterCount()-1 )
            throw RuntimeX.mWrongNumberOfArguments(
                    method.getParameterCount(),
                    args.length );

        final int variadicPosition =
                method.getParameterCount() - 1;

        Class<?>[] initTypes =
                method.getParameterTypes();
        Object[] initargs =
                new Object[ initTypes.length ];

        // Map the parameters before the variadic position.
        for ( int i = 0 ; i < variadicPosition ; i++ )
            initargs[i] = map( args[i], initTypes[i] );

        if ( args.length == variadicPosition )
        {
            // Called without an variadic element.
            initargs[variadicPosition] = null;
        }
        else if ( args.length == 1 + variadicPosition )
        {
            initargs[variadicPosition] = map(
                    args[args.length-1],
                    initTypes[variadicPosition] );
        }
        else
        {
            initargs[variadicPosition] = mapArray(
                    initTypes[variadicPosition].componentType(),
                    Arrays.copyOfRange(
                            args,
                            variadicPosition,
                            args.length ) );
        }

        try
        {
            return method.newInstance(
                    initargs );
        }
        catch ( InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e )
        {
            throw RuntimeX.mCreationFailed( method.getName() );
        }
    }

    public static Object createInstance( Constructor<?> ctor,
            FirstClassObject[] args ) throws RuntimeX
    {
        if ( ctor.isVarArgs() )
            return createInstanceVariadic( ctor, args );

        if ( args.length != ctor.getParameterCount() )
            throw RuntimeX.mWrongNumberOfArguments( ctor.getParameterCount(), args.length );

        Class<?>[] initTypes =
                ctor.getParameterTypes();
        Object[] initargs =
                new Object[ initTypes.length ];


        for ( int i = 0 ; i < initTypes.length ; i++ )
            initargs[i] = map( args[i], initTypes[i] );

        try
        {
            return ctor.newInstance( initargs );
        }
        catch ( InstantiationException | IllegalAccessException
                | IllegalArgumentException | InvocationTargetException e )
        {
            throw RuntimeX.mCreationFailed( ctor.getName() );
        }
    }

    public static Object createInstance( String ctorSpec,
            FirstClassObject[] args ) throws RuntimeX
    {
        return createInstance( getCtor( ctorSpec ), args );
    }

    private Object callVariadic(
            SchemeObject instance,
            Method method,
            FirstClassObject[] args )
        throws RuntimeX
    {
        if ( args.length < method.getParameterCount()-1 )
            throw RuntimeX.mWrongNumberOfArguments(
                    method.getParameterCount(),
                    args.length );

        final int variadicPosition =
                method.getParameterCount() - 1;

        Class<?>[] initTypes =
                method.getParameterTypes();
        Object[] initargs =
                new Object[ initTypes.length ];

        // Map the parameters before the variadic position.
        for ( int i = 0 ; i < variadicPosition ; i++ )
            initargs[i] = map( args[i], initTypes[i] );

        if ( args.length == variadicPosition )
        {
            // Called without an variadic element.
            initargs[variadicPosition] = null;
        }
        else if ( args.length == 1 + variadicPosition )
        {
            initargs[variadicPosition] = map(
                    args[args.length-1],
                    initTypes[variadicPosition] );
        }
        else
        {
            initargs[variadicPosition] = mapArray(
                    initTypes[variadicPosition].componentType(),
                    Arrays.copyOfRange(
                            args,
                            variadicPosition,
                            args.length ) );
        }

        try
        {
            return method.invoke(
                    instance.toJava(),
                    initargs );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, method );
        }
        catch ( IllegalAccessException | IllegalArgumentException e )
        {
            throw RuntimeX.mInvocationException( method, e );
        }
    }

    private Object callImpl(
            SchemeObject instance,
            Method method,
            FirstClassObject[] args ) throws RuntimeX
    {
        if ( method.isVarArgs() )
            return callVariadic( instance, method, args );

        if ( args.length != method.getParameterCount() )
            throw RuntimeX.mWrongNumberOfArguments( method.getParameterCount(), args.length );

        Class<?>[] initTypes =
                method.getParameterTypes();
        Object[] initargs =
                new Object[ initTypes.length ];

        for ( int i = 0 ; i < initTypes.length ; i++ )
            initargs[i] = map( args[i], initTypes[i] );

        try
        {
            return method.invoke(
                    instance.toJava(),
                    initargs );
        }
        catch ( InvocationTargetException e )
        {
            throw filterException( e, method );
        }
        catch ( IllegalAccessException | IllegalArgumentException e )
        {
            throw RuntimeX.mInvocationException( method, e );
        }
    }

    public Object call(
            SchemeObject instance,
            String methodSpec,
            FirstClassObject[] args ) throws RuntimeX
    {
        return callImpl(
                instance,
                getMethod( methodSpec ),
                args  );
    }

    private static long assertImpl( Int number, long min, long max )
            throws RuntimeX
    {
        var value =  number.asLong();

        if ( value < min || value > max )
            throw RuntimeX.mRangeExceeded(
                    number,
                    String.format( "[%d..%d]", min, max ) );

        return value;
    }

    private static byte assertByte( Int number )
            throws RuntimeX
    {
        return (byte)assertImpl( number, Byte.MIN_VALUE, Byte.MAX_VALUE );
    }

    private static short assertShort( Int number )
            throws RuntimeX
    {
        return (short)assertImpl( number, Short.MIN_VALUE, Short.MAX_VALUE );
    }

    private static int assertInt( Int number )
            throws RuntimeX
    {
        return (int)assertImpl( number, Integer.MIN_VALUE, Integer.MAX_VALUE );
    }

    /**
     *
     * @param spec "aName:float,int"
     * @return Pair{ "aName", "(float,int)" }
     * @throws RuntimeX
     */
    private static Pair<String,String> makeNameArguments( String spec )
        throws RuntimeX
    {
        if ( StringUtil.isEmpty( spec ) )
            throw RuntimeX.mIllegalArgument( spec );

        var split = spec.split( ":" );

        if ( split.length > 2 )
            throw RuntimeX.mIllegalArgument( spec );

        return new Pair<String,String>(
                split[ 0 ],
                String.format( "(%s)",
                split.length == 1 ?
                        StringUtil.EMPTY_STRING :
                        split[ 1 ] ) );
    }

    /**
     * This method is responsible for handling
     * {@code InvocationTargetException}s.  This means that in
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
    private static RuntimeX filterException( InvocationTargetException ite,
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


}
