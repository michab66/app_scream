/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamEvaluator;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * {@code r7rs 6.11 Exceptions p54}
 */
public abstract class R7RsExceptions_6_11
{
    private R7RsExceptions_6_11()
    {
        throw new AssertionError();
    }

    static public final Procedure withExceptionHandlerProc( Environment closure )
    {
        return new Procedure( "with-exception-handler", closure )
        {
            @Override
            public Thunk _apply( Cons args,
                    Cont<FirstClassObject> c )
                throws RuntimeX
            {
                checkArgumentCount( 2, args );

                Procedure handler =
                        Scut.as( Procedure.class, args.listRef( 0 ) );
                Procedure thunk =
                        Scut.as( Procedure.class, args.listRef( 1 ) );

                Cont<FirstClassObject> _exit = result -> {
                    return () -> { throw RuntimeX.mNotContinuable(); };
                };

                Cont<RuntimeX> _handler = rx -> {
                    if ( rx.getCode() == Code.RAISE )
                    {
                        // Pop the exception handler so that errors in the
                        // handler procedure propagate to the parent exception
                        // handler.
                        ScreamEvaluator.CONT.get().popExceptionHandler();
                        return handler.execute(
                                (Environment)rx.getArgument( 0 ),
                                (Cons)rx.getArgument(1),
                                _exit );
                    }

                    return () -> { throw rx; };
                };

                ScreamEvaluator.CONT.get().pushExceptionHandler( _handler );

                return thunk.apply(
                        Cons.NIL,
                        // This is the continuation taken if the
                        // thunk terminated without throwing.
                        result -> {
                            ScreamEvaluator.CONT.get().popExceptionHandler();
                            return c.accept( result );
                        });
            }
        };
    }

    static public final Procedure raiseProc( Environment e )
    {

        return new Procedure( "raise", e )
        {
            @Override
            public Thunk _execute( Environment e, Cons args,
                    Cont<FirstClassObject> c ) throws RuntimeX
            {
                checkArgumentCount( 1, args );

                return () ->  { throw RuntimeX.mRaise( e, args ); };
            }
        };
    }

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( withExceptionHandlerProc( tle ) );
        tle.setPrimitive( raiseProc( tle ) );

        return tle;
    }
}
