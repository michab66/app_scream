/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.Scut;

/**
 * The continuation-type that is passed as an exit procedure into the
 * procedure received by {@code call-with-current-continuation}.
 *
 * @author micbinz
 */
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
    static private Procedure callccProc( Environment e )
    {
        return new Procedure( "call-with-current-continuation" )
        {
            @Override
            protected Thunk __executeImpl(
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
        }.setClosure( e );
    }

    /**
     * (call-with-current-continuation ...
     */
    static private Procedure callWithValuesProc( Environment e )
    {
        return new Procedure( "call-with-values" )
        {

            @Override
            protected Thunk __executeImpl(
                    Environment e,
                    Cons args,
                    Cont<FirstClassObject> c )
                            throws RuntimeX
            {

                checkArgumentCount( 2, args );

                var producer = Scut.as(
                        Procedure.class,
                        args.listRef(0) );
                var consumer = Scut.as(
                        Procedure.class,
                        args.listRef(1) );

                Cont<Cons> branch = values ->
                {
                    return consumer._execute(
                            e,
                            values,
                            c );
                };

                return producer._execute(
                        e,
                        Cons.NIL,
                        values -> Primitives._cast(
                                Cons.class,
                                values,
                                branch ) );
            }
        }.setClosure( e );
    }

    @Override
    protected Thunk __executeImpl( Environment e, Cons args,
            Cont<FirstClassObject> c ) throws RuntimeX
    {
        var values = args.length() == 1 ?
                args.getCar() :
                args;

        return _cont.accept( values );
    }

    /**
     * Operations setup.
     *
     * @param tle The toplevel-environment to extend.
     * @return The extended environment.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( callWithValuesProc( tle ) );
        var ccc = callccProc( tle );
        tle.setPrimitive( ccc );
        tle.define( Symbol.createObject( "call/cc" ), ccc );

        return tle;
    }
}
