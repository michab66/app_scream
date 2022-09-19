/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package urschleim;

import java.util.function.Consumer;
import java.util.logging.Logger;

import de.michab.scream.RuntimeX;

public class Continuation
{
    private static Logger LOG =
            Logger.getLogger( Continuation.class.getName() );


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

    public static <T> Cont<T> endCall(Consumer<T> call) {
        return r -> {
            call.accept(r);
            return null;
        };
    }

}
