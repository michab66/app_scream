/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream;

import java.util.function.Consumer;

import org.smack.util.Holder;

/**
 * Continuation infrastructure.
 *
 * @author micbinz
 */
public class Continuation
{
    private static int _thunkCount;

    /**
     * Hide ctor.
     */
    private Continuation()
    {
        throw new AssertionError();
    }

    @FunctionalInterface
    public interface ToStackOp<T> {
        Thunk call( Cont<T> c )
            throws Exception;
    }

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws Exception;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws Exception;
    }

    /**
     *
     * @param <X>
     * @param t The thunk to execute.
     * @param xCont The continuation to take in case of an exception.
     * @param xClass The exception class that is to be forwarded to xCont.
     * @throws Exception If an exception different from xClass happens or if
     * xCont threw an exception.
     */
    private static <X extends Exception>
    void trampoline(
            Thunk t,
            Cont<X> xCont,
            Class<X> xClass )
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
                if ( xClass.isAssignableFrom( e.getClass() ) )
                    // ... then the passed exception continuation is called.
                    // If this in turn throws an exception then thunk processing
                    // is terminated.
                    t = xCont.accept( xClass.cast( e ) );
                else
                    // Otherwise terminate.
                    throw e;
            }
        }
    }

    private static <T> Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

    /**
     * @return The current value of the thunk counter.
     */
    public static int thunkCount()
    {
        return _thunkCount;
    }

    /**
     * Set the thunk counter.  Used in debugging.
     *
     * @param newValue The new value for the thunk counter.
     */
    public static void thunkCount( int newValue )
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
     * @param exceptionHandler The exception handler.
     * @param exceptionClass The type of the exception that is handled.
     * @return The result of the passed operation.
     * @throws Exception
     */
    public static <T, X extends Exception>
    T toStack(
            ToStackOp<T> op,
            Cont<X> exceptionHandler,
            Class<X> exceptionClass )
        throws Exception
    {
        Holder<T> result =
                new Holder<>();

        trampoline(
                op.call( endCall( s -> result.set( s ) ) ),
                exceptionHandler,
                exceptionClass );

        return result.get();
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
    public static <T>
    T toStack(
            ToStackOp<T> op )
        throws Exception
    {
        Holder<T> result =
                new Holder<>();
        Holder<Exception> exception =
                new Holder<>();

        Cont<Exception> handler =
                ae -> {
                    exception.set( ae );
                    return null;
                };

        trampoline(
                op.call( endCall( s -> result.set( s ) ) ),
                handler,
                Exception.class );

        if ( exception.get() != null )
            throw exception.get();

        return result.get();
    }
}
