/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2022 Michael G. Binz
 */
package urschleim;

import java.util.function.Consumer;
import java.util.logging.Logger;

import de.michab.scream.Cons;
import de.michab.scream.Environment;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException;

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

    public static void trampoline( Thunk t, Consumer<ScreamException> err )
    {
        try
        {
            while (t != null) {
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

    public static Thunk _eval(
            Environment e,
            FirstClassObject o,
            Cont<FirstClassObject> c )
    {
        return () ->
            c.accept(
                    FirstClassObject.evaluate( o, e ) );
    }

    private static Thunk _begin(
            Environment e,
            Cons body,
            FirstClassObject previousResult,
            Cont<FirstClassObject> c )
    {
        if ( body == Cons.NIL )
            return () -> c.accept( previousResult );

        Cont<FirstClassObject> next =
                (fco) -> _begin( e, (Cons)body.getCdr(), fco, c);

        return () -> _eval( e, body.getCar(), next );
    }

    public static Thunk _begin(
            Environment e,
            Cons body,
            Cont<FirstClassObject> c )
    {
        return _begin(
                e,
                body,
                Cons.NIL,
                c );
    }

    public static Thunk _if(
            boolean expr,
            Cont<Boolean> trueBranch,
            Cont<Boolean> falseBranch)
    {
        return (expr)
                ? () -> trueBranch.accept(true)
                : () -> falseBranch.accept(false);
    }

    private static Thunk listEval( Environment e, int i, FirstClassObject[] l, Cont<FirstClassObject[]> c )
    {
        if ( i == l.length )
            return () -> c.accept( l );

        return () -> {
            if ( l[i]  != Cons.NIL )
                l[i] = FirstClassObject.evaluate( l[i], e );
            return listEval( e, i+1, l, c );
        };
    }

    public static Thunk listEval( Environment e, FirstClassObject[] l, Cont<FirstClassObject[]> c )
    {
        return () -> listEval( e, 0, l, c );
    }
}
