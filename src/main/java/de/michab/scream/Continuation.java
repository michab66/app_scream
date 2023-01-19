/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022-2023 Michael G. Binz
 */
package de.michab.scream;

import java.util.function.Consumer;
import java.util.logging.Logger;

import org.smack.util.Holder;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;

/**
 * Continuation infrastructure.
 *
 * @author micbinz
 */
public class Continuation
{
    private final static Logger LOG = Logger.getLogger(
            Continuation.class.getName() );

    private static int _thunkCount;

    /**
     * Hide ctor.
     */
    private Continuation()
    {
        throw new AssertionError();
    }

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws RuntimeX;
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
        Thunk call( Cont<FirstClassObject> c )
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
    public static FirstClassObject toStack( ToStackOp op )
        throws RuntimeX
    {
        Holder<FirstClassObject> r =
                new Holder<FirstClassObject>( Cons.NIL );
        Holder<ScreamException> error =
                new Holder<>( null );

        Continuation.trampoline(
                op.call(
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
}
