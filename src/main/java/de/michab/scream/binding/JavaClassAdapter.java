/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Logger;

import org.smack.util.JavaUtil;
import org.smack.util.Pair;
import org.smack.util.ReflectionUtil;
import org.smack.util.StringUtil;
import org.smack.util.collections.OneToN;

import de.michab.scream.RuntimeX;
import de.michab.scream.fcos.Bool;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Number;
import de.michab.scream.fcos.SchemeCharacter;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.fcos.Vector;
import de.michab.scream.util.Scut;

/**
 * Encapsulates information for an arbitrary Java class.
 *
 * @author Michael Binz
 */
public class JavaClassAdapter
{
    /**
     * The logger for this class.
     */
    private final static Logger LOG =
            Logger.getLogger( JavaClassAdapter.class.getName() );

    /**
     * Holds the already constructed ClassAdpeters.
     */
    private final static HashMap<String, JavaClassAdapter> _classAdapterInstances =
            new HashMap<String, JavaClassAdapter>();

    /**
     * The {@code java.lang.Class} this object is associated with.
     */
    private final Class<?> _clazz;

    /**
     * A map of _methods.
     * Maps a pair(name,parameterCount) to a list of methods.
     */
    private final OneToN<Pair<String, Integer>, Method, ArrayList<Method>>
    _methodMap =
            new OneToN<>( ArrayList<Method>::new );

    private final Method[] _methods;

    /**
     * Constructs a class adapter for the passed class.
     *
     * @param clazz The class to create an adapter for.
     */
    private JavaClassAdapter( Class<?> clazz )
    {
        _clazz = clazz;

        _methods = initMethods( clazz );

        for ( Method c : _methods )
            _methodMap.putValue(
                    new Pair<>( c.getName(), c.getParameterCount() ),
                    c );
    }

    /**
     * A map holding already resolved methods.
     */
    private final Map<String,Method> _methodCache =
            new HashMap<>();

    private Method getMethodImpl( String name )
            throws RuntimeX
    {
        ArrayList<Method> result = new ArrayList<>();

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

            result.add( c );
        }

        if ( result.isEmpty() )
            throw RuntimeX.mMethodNotFound( nameArguments.left + braced );
        if ( result.size() > 1 )
            throw RuntimeX.mInternalError( Symbol.createObject( "notUnique" ) );

        return result.get( 0 );
    }

    public Method getMethod( String name  )
            throws RuntimeX
    {
        if ( ! _methodCache.containsKey( name ) )
            _methodCache.put( name, getMethodImpl( name ) );

        return _methodCache.get( name );
    }

   /**
    * A map holding already resolved constructors.
    */
   private static final Map<String,Constructor<?>> _ctorCache =
           new HashMap<>();

   private static Constructor<?> getCtorImpl( String name )
           throws RuntimeX
   {
       var nameArguments = JavaClassAdapter.makeNameArguments( name );

       var cl = get( nameArguments.left );

       ArrayList<Constructor<?>> result = new ArrayList<>();

       for ( var c : cl._clazz.getConstructors() )
       {
           if ( ! c.getName().equals( nameArguments.left ) )
               continue;

           if ( ! c.toString().contains( nameArguments.right ) )
               continue;

           if ( c.isSynthetic() )
               continue;

           result.add( c );
       }

       if ( result.isEmpty() )
           throw RuntimeX.mMethodNotFound( nameArguments.left + "<init>" + nameArguments.right );
       if ( result.size() > 1 )
           throw RuntimeX.mInternalError( Symbol.createObject( "notUnique" ) );

       return result.get( 0 );
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
       if ( ! _ctorCache.containsKey( name ) )
           _ctorCache.put( name, getCtorImpl( name ) );

       return _ctorCache.get( name );
   }

   /**
     *
     */
    private static Method[] initMethods( Class<?> clazz )
    {
        HashMap<String, Method> methods =
                new HashMap<String, Method>();

        // There is no simple way
        // to get all methods.  getMethods() below returns all public methods
        // including the inherited ones, while getDeclaredMethods() returns *all*
        // existing methods on the class but excludes the inherited ones.
        for ( Method c : clazz.getMethods() )
        {
            if ( c.isSynthetic() )
                continue;

            // For methods the method name also has to be part of the mangled name.
            String mangled =
                    c.getName() +
                    "/" +
                    mangleArguments( c.getParameterTypes() );

            Method previous = methods.get( mangled );
            // If we have a method with a similar signature...
            if ( previous != null )
                // ...we have to decide which one to use and set this.
                methods.put( mangled, selectMethod( previous, c ) );
            else
                methods.put( mangled, c );
        }

        // Now the hashtable contains the filtered set of Scream-callable
        // methods. Finally filter the methods a last time so that all methods in
        // the array are really callable.
        return ensureCallAccess(
                methods.values().toArray( new Method[ methods.size() ] ) );
    }

    /**
     * Get a class adapter instance.
     *
     * @param cl The class to create an adapter for.
     * @return A class adapter for the passed class.
     */
    static JavaClassAdapter get( Class<?> cl )
    {
        Objects.requireNonNull( cl );

        final String key = cl.getName();

        // Check if we have a class adapter for this class.
        JavaClassAdapter result =
                _classAdapterInstances.get( key );

        if ( null == result )
        {
            // We had no adapter.  So create one...
            result = new JavaClassAdapter( cl );
            // ...and put it in the hash table.
            _classAdapterInstances.put( key, result );
        }

        return result;
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
            throw SchemeObject.filterException( e, c );
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

    public List<Method> getMethods( String name, int parameterCount )
    {
        return _methodMap.getValues(
                new Pair<>( name, parameterCount ) );
    }
    public  Method[] getMethods()
    {
        return _methods;
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

    /**
     * Computes the methods that can be called on an actual class instance.
     * <br>
     *
     * Reflection FAQ (https://www2.ki.informatik.uni-frankfurt.de/doc/html/java/jdk115/guide/reflection/faq/faq.html):
     * It is a common error to attempt to invoke an overridden
     * method by retrieving the overriding method from the target object. This
     * will not always work, because the overriding method will in general be
     * defined in a class inaccessible to the caller. For example, the following
     * code only works some of the time, and will fail when the target object's
     * class is too private:
     * {@code
     *    void invokeCommandOn(Object target, String command) {
     *      try {
     *            Method m = target.getClass().getMethod(command, new Class[] {});
     *            m.invoke(target, new Object[] {});
     *      } catch ...
     *    } }
     *
     * The workaround is to use a much more complicated algorithm, which starts
     * with target.getClass() and works up the inheritance chain, looking for a
     * version of the method in an accessible class.
     *
     * This method implements that much more complicated algorithm.
     *
     * @param methods The array of methods to be checked on callability.  In case
     *                an alternate method is identified for calling this will be
     *                directly entered into the original array, replacing the
     *                existing but uncallable method.
     * @return The methods that are allowed to be called on an actual class
     *         instance.
     */
    private static Method[] ensureCallAccess( Method[] methods )
    {
        for ( int i = methods.length -1 ; i >= 0 ; i-- )
        {
            if ( Modifier.isPublic( methods[i].getDeclaringClass().getModifiers() ) )
                continue;

            methods[i] = findCallable( methods[i].getDeclaringClass(), methods[i] );
        }

        return methods;
    }

    /**
     * Find a callable representation of the passed method in the inheritance
     * tree of the passed class.
     * @param c The class representing one node in the inheritance tree that is
     *          searched.
     * @param m The method to look for.
     * @return A callable alternative for m or in case no alternative was found
     *         a unmodified reference to m.
     */
    private static Method findCallable( Class<?> c, Method m )
    {
        // Note: That guy is recursive.  Strategy is to search the passed class
        // and its interfaces for a method with the same signature like m.  If
        // nothing is found, we go into recursion with the superclass of c.
        // If we reached java.lang.object the superclass is null and that is the
        // final break condition for the recursion.

        // Check recursion break.
        if ( c == null )
        {
            LOG.warning( "JavaClassAdapter.findCallable: " +
                    "No replacement found.  Return original." );
            return m;
        }

        // Get the interfaces of the passed class and search through them.
        Class<?>[] interfaces = c.getInterfaces();
        for ( int i = interfaces.length -1 ; i >= 0 ; i-- )
        {
            if ( Modifier.isPublic( interfaces[i].getModifiers() ) )
            {
                try
                {
                    m = interfaces[i].getMethod( m.getName(), m.getParameterTypes() );
                    // Found it!  Leave...
                    return m;
                }
                catch ( NoSuchMethodException e )
                {
                    // No success here.  Continue with the search.
                }
            }
        }

        // Check if the passed class itself has a callable version of the method.
        if ( Modifier.isPublic( c.getModifiers() ) )
        {
            try
            {
                m = c.getMethod( m.getName(), m.getParameterTypes() );
                // Found it!  Leave...
                return m;
            }
            catch ( NoSuchMethodException e )
            {
                // No success here.  Continue with the search.
            }
        }

        // Not found so far.  Let's try it on the Superclass
        return findCallable( c.getSuperclass(), m );
    }

    /**
     * Decides which of the two passed methods is cheaper to call from
     * scream and returns this.
     *
     * @param l The first method.
     * @param r The second method.
     * @return The method that is more efficient to call.
     */
    private static Method selectMethod( Method l, Method r )
    {
        boolean which = selectArgumentList( l.getParameterTypes(),
                r.getParameterTypes() );

        // Return value of selectArgumentList has to be interpreted as integer, so
        // false = 0 and true = 1.
        if ( ! which )
        {
            System.out.println( "Select " + l );
            System.out.println( "Delete " + r );
            return l;
        }
        else
        {
            System.out.println( "Select " + r );
            System.out.println( "Delete " + l );
            return r;
        }
    }

    /**
     * Decides which of the two argument lists is cheaper to provide from
     * scream and returns the according index in boolean form.
     *
     * @param l The first argument list.
     * @param r The second argument list.
     * @return true if the first arglist should be used, false otherwise.
     */
    private static boolean selectArgumentList( Class<?>[] l, Class<?>[] r )
    {
        JavaUtil.Assert( l.length == r.length );

        for ( int i = 0 ; i < l.length ; i++ )
        {
            if ( l[i] != r[i] )
            {
                int am = mapJavaNumber( l[i] );
                int bm = mapJavaNumber( r[i] );

                return am < bm;
            }
        }

        // Signatures were 100% equal? Should be not possible.
        LOG.warning( "Assertion failed: selectArgumentList 2" );
        return false;
    }

    /**
     * <p>A helper method, simply mapping the Java-defined primitive numeric
     * types to integer numbers.  Smaller sizes of the type result in a smaller
     * number.</p>
     * <p>The method handles all numeric types including arrays of numeric types.
     * Arrays are mapped to their component type.  It is explicitly undefined
     * which number is returned for what numeric type.</p>
     *
     * @param formal The class to map.  Has to be either one of the primitive
     *        numeric type representations or an array of these.
     * @return A corresponding integer.
     */
    private static int mapJavaNumber( Class<?> formal )
    {
        int result = Integer.MIN_VALUE;

        if ( formal == java.lang.Byte.TYPE )
            result = 1;
        else if ( formal == java.lang.Short.TYPE )
            result = 2;
        else if ( formal == java.lang.Integer.TYPE )
            result = 3;
        else if ( formal == java.lang.Long.TYPE )
            result = 4;
        else if ( formal == java.lang.Float.TYPE )
            result = 5;
        else if ( formal == java.lang.Double.TYPE )
            result = 6;
        else if ( formal.isArray() )
            result = mapJavaNumber( formal.getComponentType() );
        else
            LOG.severe( "Map number failed." );

        return result;
    }

    /**
     * Mangles a passed argument list into a string.  Used in general for typesafe
     * linking with old linkers.  But here we use signature mangling to be able to
     * detect similar signatures, being different only in the numeric formal args.
     *
     * @param formals The argument list to mangle.
     * @return The mangled argument list.
     */
    private static String mangleArguments( Class<?>[] formals )
    {
        StringBuffer result = new StringBuffer();

        // For each formal specified...
        for ( int i = 0 ; i < formals.length ; i++ )
            // ...append its mangled representation.
            result.append( mangleFormal( formals[i]  ) );

        return result.toString();
    }

    /**
     * Mangles a single formal argument.  This maps java's integer and floating
     * point primitive types to a common tag.
     *
     * @param formal The class to be mangled.
     * @return The mangled class name.
     */
    private static String mangleFormal( Class<?> formal )
    {
        if ( formal == java.lang.Byte.TYPE ||
                formal == java.lang.Short.TYPE ||
                formal == java.lang.Integer.TYPE ||
                formal == java.lang.Long.TYPE )
            // Integer
            return "I";

        else if ( formal == java.lang.Float.TYPE ||
                formal == java.lang.Double.TYPE )
            // Real
            return "R";

        else if ( formal == java.lang.Character.TYPE )
            // Character
            return "C";

        else if ( formal == java.lang.Boolean.TYPE )
            // Boolean
            return "B";

        else if ( formal.isArray() )
            return "[" + mangleFormal( formal.getComponentType() );

        else
            // Object
            return "@" + formal.getName();
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
        catch ( IllegalAccessException | IllegalArgumentException | InvocationTargetException e )
        {
            throw RuntimeX.mInvocationException( method, e );
        }
    }

    public Object call(
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
            throw SchemeObject.filterException( e, method );
        }
        catch ( IllegalAccessException | IllegalArgumentException e )
        {
            throw RuntimeX.mInvocationException( method, e );
        }
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
     * @return Pair{ "aName", "(float,int) }
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
}
