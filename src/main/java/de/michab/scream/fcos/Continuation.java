/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

public class Continuation extends Procedure
{
    private final Cont<FirstClassObject> _cont;

    /**
     * Create an instance.
     *
     * @param continuation The continuation to execute.
     */
    public Continuation( Cont<FirstClassObject> continuation )
    {
        super( "callcc" );
        _cont = continuation;
    }

    /**
     * (call-with-current-continuation ...
     */
    static private Procedure callccProc =
            new Procedure( "call-with-current-continuation" )
    {
        @Override
        protected Thunk _executeImpl(
                Environment e,
                Cons args,
                Cont<FirstClassObject> c )
            throws RuntimeX
        {
            checkArgumentCount( 1, args );

            var proc = Scut.as(
                    Procedure.class,
                    args.getCar() );

            return proc._execute(
                    e,
                    Cons.create(
                            new Continuation( c ) ),
                    c );
        }
    };

    @Override
    protected Thunk _executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        checkArgumentCount( 1, args );

        return _cont.accept( args.getCar() );
    }

    /**
     * Operations setup.
     *
     * @param tle The toplevel-environment to extend.
     * @return The extended environment.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.setPrimitive( callccProc.setClosure( tle ) );

        return tle;
    }
}
