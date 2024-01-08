/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2022-2024 Michael G. Binz
 */
package de.michab.scream.util;

import java.util.Objects;
import java.util.Stack;
import java.util.function.Consumer;

import org.smack.util.Holder;

/**
 * Continuation infrastructure.
 *
 * @author micbinz
 */
public class Continuation<T, X extends Exception>
{
    private long _thunkCount;

    private final Class<X> _exceptionClass;

    /**
     * Will receive the result for all continuation invocations.
     */
    private final Holder<T> _result = new Holder<>();

    /**
     * Create an instance.
     *
     * @param exceptionClass The class of the handled exception type.
     */
    public Continuation( Class<X>  exceptionClass )
    {
        _exceptionClass = exceptionClass;
    }

    @FunctionalInterface
    public interface ToStackOp<T> {
        Thunk call( Cont<T> c )
            throws Exception;
    }

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result);
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws Exception;
    }

    private Stack<Cont<X>> _exceptionHandlers = new Stack<>();

    /**
     *
     * @param <X>
     * @param t The thunk to execute.
     * @param xCont The continuation to take in case of an exception.
     * @param xClass The exception class that is to be forwarded to xCont.
     * @throws Exception If an exception different from xClass happens or if
     * xCont threw an exception.
     */
    private void trampoline(
            Thunk t )
                    throws Exception
    {
        while ( t != null )
        {
            _thunkCount++;

            try
            {
                t = t.run();
            }
            catch ( Exception e )
            {
                // If the exception is of type xClass ...
                if ( _exceptionClass.isAssignableFrom( e.getClass() ) )
                    // ... then the passed exception continuation is called.
                    // If this in turn throws an exception then thunk processing
                    // is terminated.
                    t = _exceptionHandlers.peek().accept( _exceptionClass.cast( e ) );
                else
                    // Otherwise terminate.
                    throw e;
            }
        }
    }

    private Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

    /**
     * @return The current value of the thunk counter. Used
     * in testing.
     */
    public long thunkCount()
    {
        return _thunkCount;
    }

    /**
     * Set the thunk counter.  Used in debugging.
     *
     * @param newValue The new value for the thunk counter.
     */
    public void thunkCount( int newValue )
    {
        _thunkCount = newValue;
    }

    /**
     * Execute a continuation-based operation with an exception handler and
     * return the result on the stack.
     *
     * @param <T> The result type.
     * @param <X> The type of the exception that is handled.
     * @param op The operation to execute.
     * @param exceptionHandler The exception handler.  Must not
     * be {@code null}.
     * @param exceptionClass The type of the exception that is handled.
     * @return The result of the passed operation.
     * @throws Exception
     */
    public
    T toStack(
            ToStackOp<T> op,
            Cont<X> exceptionHandler )
        throws Exception
    {
        Objects.requireNonNull( exceptionHandler );

        _result.set( null );

        try
        {
            pushExceptionHandler( exceptionHandler );

            trampoline(
                    op.call( endCall( s -> _result.set( s ) ) ) );

            return _result.get();
        }
        finally {
            popExceptionHandler();
        }
    }

    /**
     * Execute a continuation-based operation and
     * return the result on the stack.
     *
     * @param <T> The result type.
     * @param op The operation to execute.
     * @return The result of the passed operation.
     * @throws Exception
     */
    public
    T toStack( ToStackOp<T> op )
        throws Exception
    {
        Holder<X> exception = new Holder<>();

        Cont<X> handler =
                ae -> {
                    exception.set( ae );
                    return null;
                };

        toStack( op, handler );

        if ( exception.get() != null )
            throw exception.get();

        return _result.get();
    }

    public void pushExceptionHandler( Cont<X> handler )
    {
        _exceptionHandlers.push(
                Objects.requireNonNull( handler ) );
    }

    public Cont<X> popExceptionHandler()
    {
        return _exceptionHandlers.pop();
    }
}
