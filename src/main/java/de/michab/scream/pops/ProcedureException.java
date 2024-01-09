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
 * {@code (and <test₁> ...)} syntax
 * <p>
 * {@code r7rs 4..2.1 p15}
 */
public abstract class ProcedureException extends Procedure
{
    private ProcedureException( String name )
    {
        super( name );
    }

    static public final Procedure withExceptionHandlerProcedure = new ProcedureException( "with-exception-handler" )
    {
        @Override
        protected Thunk __executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 2, args );

            Procedure handler =
                    Scut.as( Procedure.class, args.listRef( 0 ) );
            Procedure thunk =
                    Scut.as( Procedure.class, args.listRef( 1 ) );

            Cont<FirstClassObject> _exit = result -> {
                ScreamEvaluator.CONT.get().popExceptionHandler();
                return c.accept( result );
            };

            Cont<RuntimeX> _handler = rx -> {
                if ( rx.getCode() == Code.RAISE )
                {
                    return handler.apply(
                            (Environment)rx.getArgument( 0 ),
                            new Cons( (FirstClassObject)rx.getArgument(1) ),
                            _exit );
                }

                return () -> { throw rx; };
            };

            ScreamEvaluator.CONT.get().pushExceptionHandler( _handler );

            return thunk.apply( e, Cons.NIL, _exit );
        }
    };

    static public final Procedure raise = new ProcedureException( "raise" )
    {
        @Override
        protected Thunk __executeImpl( Environment e, Cons args,
                Cont<FirstClassObject> c ) throws RuntimeX
        {
            checkArgumentCount( 1, args );

            return () -> {
                throw RuntimeX.mRaise( e, args.getCar() );
            };
        }
    };

    /**
     * Base operations setup.
     *
     * @param tle A reference to the environment to be extended.
     * @return The extended environment.
     */
    public static Environment extendNullEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( withExceptionHandlerProcedure );
        tle.setPrimitive( raise );

        return tle;
    }
}
