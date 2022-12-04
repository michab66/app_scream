/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package de.michab.scream;

import java.util.function.Consumer;

/**
 * public _x_... are externally visible primitives.
 * These always return an indirect thunk () -> ...
 * @author micbinz
 */
public class Continuation
{
    private static int _thunkCount;

    public Continuation( Consumer<RuntimeX> errorHandler )
    {
        _errorHandler = errorHandler;
    }

    Consumer<RuntimeX> _errorHandler;

    @FunctionalInterface
    public static interface Cont<R> {
        Thunk accept(R result) throws RuntimeX;
    }

    @FunctionalInterface
    public static interface Thunk {
        Thunk run() throws RuntimeX;
    }

    public void trampoline(Thunk thunk)
    {
        try
        {
            while (thunk != null) {
                thunk = thunk.run();
            }
        }
        catch ( RuntimeX e )
        {
            _errorHandler.accept( e );
        }
    }

    public static void trampoline( Thunk t, Consumer<ScreamException> err )
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

    public static <T> Cont<T> endCall(Consumer<T> call) {
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

}
