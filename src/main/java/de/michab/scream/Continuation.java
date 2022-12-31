/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import java.util.Objects;
import java.util.function.Consumer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.smack.util.Holder;

/**
 * public _x_... are externally visible primitives.
 * These always return an indirect thunk () -> ...
 * @author micbinz
 */
public class Continuation
{
    private final static Logger LOG = Logger.getLogger(
            Continuation.class.getName() );

    private Consumer<Exception> _errorHandler;

    private static int _thunkCount;

    /**
     * Create a continuation with a default exception handler.
     */
    public Continuation()
    {
        _errorHandler = x ->
            LOG.log( Level.SEVERE, "Default exception handler called.", x );
    }

    /**
     * Create a continuation with a user-defined error handler.
     *
     * @param errorHandler The error handler called if an exception occurs.
     */
    public Continuation( Consumer<Exception> errorHandler )
    {
        _errorHandler = Objects.requireNonNull( errorHandler );
    }

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws RuntimeX;
    }

    private void trampoline(Thunk thunk)
    {
        try
        {
            while (thunk != null) {
                _thunkCount++;
                thunk = thunk.run();
            }
        }
        catch ( Exception e )
        {
            _errorHandler.accept( e );
        }
    }

    private static void trampoline( Thunk t, Consumer<ScreamException> err )
    {
        try
        {
            while (t != null) {
                _thunkCount++;
                t = t.run();
            }
        }
        catch ( RuntimeX e )
        {
            err.accept( e );
        }
    }

    private static <T> Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

    public static int thunkCount()
    {
        return _thunkCount;
    }
    public static void thunkCount( int newValue )
    {
        _thunkCount = newValue;
    }

    @FunctionalInterface
    public interface ToStackOp {
        Thunk call( Environment e, Cont<FirstClassObject> c )
            throws RuntimeX;
    }

    /**
     * Execute a continuation-based operation and return the result on the
     * stack.
     * <p>
     * An example operation is {code Thunk doIt( Cont&lt;int&gt; ) throws Exception;}
     * <p>
     * This needs to be generalized.
     *
     * @param e The environment to use.
     * @param op The operation to execute.
     * @return The operation result.
     * @throws Exception in case of an error.
     */
    public static FirstClassObject toStack( Environment e, ToStackOp op )
        throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                op.call( e,
                        Continuation.endCall( s -> r.set( s ) ) ),
                s -> error.set( s ) );

        if ( error.get() != null )
            throw (RuntimeX)error.get();

        return r.get();
    }

    @FunctionalInterface
    public interface ToStackOpx<T> {
        Thunk call( Cont<T> c )
            throws Exception;
    }

    /**
     * Execute a continuation-based operation and return the result on the
     * stack.
     * <p>
     * An example operation is {code Thunk doIt( Cont&lt;int&gt; ) throws Exception;}
     *
     * @param <T> The operation result type.
     * @param op The operation to execute.
     * @return The operation result.
     * @throws Exception in case of an error.
     */
    public <T> T toStack( ToStackOpx<T> op )
        throws Exception
    {
        Holder<T> r =
                new Holder<T>();
        trampoline(
                op.call( Continuation.endCall( s -> r.set( s ) ) ) );
        return r.get();
    }
}
