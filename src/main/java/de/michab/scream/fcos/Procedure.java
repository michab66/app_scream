/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import de.michab.scream.RuntimeX;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;

/**
 * Represents a Scheme procedure.
 */
public class Procedure
    extends Operation
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "procedure";

    /**
     * This procedure's effective environment.
     *
     * @label effective environment
     */
    private final Environment _closure;

    /**
     * A constructor for Java-defined procedures.
     *
     * @param name The symbolic name for the new procedure.
     */
    protected Procedure( String name, Environment closure )
    {
        super( Symbol.createObject( name ) );

        _closure = closure;
    }

    /**
     * Constructor used to create Scheme-defined procedures.
     *
     * @param e The new procedure's closure.
     * @param args The list of formal arguments.
     * @param body The body of the new procedure.
     * @throws RuntimeX In case an error occurred.
     */
    public Procedure(
            Environment e,
            FirstClassObject args,
            Cons body  )
                    throws RuntimeX
    {
        super( args, body );

        _closure = e;
    }

    /**
     * Evaluates the arguments in the received environment and passes on to
     * {@link #_executeImpl(Environment, Cons, Cont)} for execution of the
     * _body in the procedure's closure.
     *
     * @param e
     * @param args
     * @param c
     * @return
     * @throws RuntimeX
     */
    @Override
    protected final Thunk _execute( Environment e, Cons args, Cont<FirstClassObject> c )
    {
        // Evaluate the arguments in the received environment.
        return () -> Primitives._evalCons(
                e,
                args,
                evaluated -> apply( e, evaluated, c ) );
    }

    public final Thunk apply( Environment e, Cons args, Cont<FirstClassObject> c  )
    {
        // Do not evaluate the arguments.
        return () -> _executeImpl(
                _closure,
                args,
                c );
    }
}
