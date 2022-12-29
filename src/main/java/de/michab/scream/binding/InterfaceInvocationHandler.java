/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.binding;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.logging.Logger;

import de.michab.scream.FirstClassObject;
import de.michab.scream.Operation;
import de.michab.scream.Procedure;
import de.michab.scream.RuntimeX;

/**
 * The universal invocation handler that gets associated with a dynamically
 * created interface implementation instance.  Core functionality is to provide
 * on the one hand functionality to set handler procedures for the interface
 * methods.  On the other hand implements the dispatch from method invocations
 * onto the mapped Scream procedures.
 *
 * @see InterfaceConfigurator
 * @see SchemeObject
 * @author Michael Binz
 */
public final class InterfaceInvocationHandler
    implements
        InvocationHandler,
        InterfaceConfigurator
{
    /**
     * The logger for this class.
     */
    private final static Logger LOG =
            Logger.getLogger( InterfaceInvocationHandler.class.getName() );

    /**
     * Maps methods to operations.
     */
    private final Hashtable<Method, Operation> _invocationMap =
            new Hashtable<Method, Operation>();

    /**
     * Creates a new InterfaceInvocationHandler
     */
    public InterfaceInvocationHandler()
    {
    }

    /**
     * Allows to specify the Scheme procedure that implements the given method.
     * Implements the InterfaceConfigurator interface.
     *
     * @see de.michab.scream.binding.InterfaceConfigurator
     */
    @Override
    public Operation defineOperation( Method m, Procedure o )
    {
        Operation old = _invocationMap.get( m );
        _invocationMap.put( m, o );
        return old;
    }

    /**
     * Entry point for method calls of the Proxy owning the given invocation
     * handler.  This method actually handles invocations.  Implements the
     * InvocationHandler interface.
     *
     * @see java.lang.reflect.InvocationHandler
     */
    @Override
    public Object invoke( Object proxy, Method method, Object[] args )
            throws Throwable
    {
        // Check if the called method is part of our own interface...
        Method target = mapMethod( method, args );
        if ( target != null )
            // ...and simply forward the call if it is.
            return target.invoke( this, args );

        // Remap the method to the method that was actually called.  The method
        // that is passed as a parameter is the method on the implemented
        // interface.
        Class<?> proxyClass = proxy.getClass();
        method = proxyClass.getMethod( method.getName(), method.getParameterTypes() );
        // Now check if we have a procedure that maps to this call...
        Operation op = _invocationMap.get( method );
        if ( op == null )
        {
            LOG.info( "Operation not mapped: " + method );
            return computeDummyValue( method.getReturnType() );
        }

        return null;
//                SchemeObject.convertScream2Java(
//                        method.getReturnType(),
//                        FirstClassObject.evaluate(
//                                new Cons( op, Cons.create( java2scream( args ) ) ),
//                                null ));
    }

    /**
     * Transforms the given parameter list from the java reflection system into
     * the corresponding Scream parameter list containing the Scream replacements
     * for the Java types.
     */
    private FirstClassObject[] java2scream( Object[] args )
            throws RuntimeX
    {
        FirstClassObject[] result =
                new FirstClassObject[ args != null ? args.length : 0 ];

        for ( int i = 0 ; i < result.length ; i++ )
            result[i] = SchemeObject.convertJava2Scream( args[i] ) ;

        return result;
    }

    /**
     * Tries to find a method corresponding to the passed method name and the
     * passed arguments on this implementation.  This is used to forward for
     * example calls to the Proxy's <code>toString()</code> method to the one on
     * this class.
     */
    private Method mapMethod( Method method, Object[] args )
    {
        Method result;

        try
        {
            Class<?>[] argTypes = new Class[ args != null ? args.length : 0 ];
            for ( int i = 0 ; i < argTypes.length ; i++ )
                argTypes[i] = args[i].getClass();

            result = getClass().getMethod( method.getName(), argTypes );
        }
        catch ( NoSuchMethodException e )
        {
            result = null;
        }

        return result;
    }

    /**
     * Create a value for the given class that is valid to be put into an
     * parameter slot of the passed type.  For object references this will
     * be always <code>null</code>.  For the primitives this will be a zero.
     */
    private Object computeDummyValue( Class<?> type )
    {
        Object result = null;

        if ( type == Byte.TYPE )
            result = Byte.valueOf( (byte)0 );
        else if ( type == Character.TYPE )
            result = Character.valueOf( '\0' );
        else if ( type == Short.TYPE )
            result = Short.valueOf( (short)0 );
        else if ( type == Integer.TYPE )
            result = Integer.valueOf( 0 );
        else if ( type == Long.TYPE )
            result = Long.valueOf( 0 );
        else if ( type == Float.TYPE )
            result = Float.valueOf( 0.0f );
        else if ( type == Double.TYPE )
            result = Double.valueOf( 0.0 );
        else if ( type == Boolean.TYPE )
            result = Boolean.FALSE;
        else if ( type == Void.TYPE )
            result = Void.TYPE;

        if ( type.isPrimitive() && result == null )
            LOG.severe(
                    "Internal error: " +
                            "Primitive list in " + getClass().getName() +
                            "#computeDummyValue incomplete.  Missing type: " +
                            type.getName() );

        return result;
    }
}
