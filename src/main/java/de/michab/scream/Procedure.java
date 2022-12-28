/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import org.smack.util.JavaUtil;

import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Lambda.L;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.pops.Primitives;

/**
 * Represents a scheme procedure.
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
    private Environment _closure;

    /**
     * Default constructor.  Used for Java-defined specialisations.
     *
     * @param name The symbolic name for the new procedure.
     */
    protected Procedure( Symbol name )
    {
        super( name );
        _closure = null;
    }
    public Procedure setClosure( Environment closure )
    {
        JavaUtil.Assert( _closure == null );
        _closure = closure;
        return this;
    }

    /**
     * Default constructor.  Used for Java defined specialisations.
     *
     * @param name The symbolic name for the new procedure.
     */
    protected Procedure( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     * Constructor used to create Scheme-defined procedures.
     *
     * @param e The environment to be used by the new procedure.
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
        super( args, body, e );
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
            throws RuntimeX
    {
        // Execute in our _closure.
        Cont<Cons> cc = evaluated -> _executeImpl(
                _closure,
                evaluated,
                c );

        // Evaluate the arguments in the environment that we receive.
        return () -> Primitives._x_evalCons(
                e,
                args,
                cc );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
//        checkArgumentCount( args );

        L l = (e,c) -> _execute( e, args, c );

        return new Lambda(
                l,
                this.toString() ).setInfo( args );
    }

    /**
     * Since we are a procedure the arguments are evaluated and rest of the job
     * is delegated to the apply methods.  The passed argument list has to be
     * proper.  Note that as the final step after bumping through the chain of
     * <code>apply()</code> methods our superclass' <code>activate()</code> will
     * be called for executing the Scheme defined procedure.  A procedure
     * implemented in Java has to override one of the chained
     * <code>apply()</code> methods.
     *
     * @param parent The parent environment.
     * @param argumentList The actually passed argument list.
     * @return The result of the procedure activation.
     * @throws RuntimeX In case of an error.
     */
    @Override
    public FirstClassObject activate( Environment parent, Cons argumentList )
            throws RuntimeX
    {
        // Check if our argument list is proper.
        if ( argumentList != Cons.NIL && !argumentList.isProperList() )
            throw new RuntimeX( Code.EXPECTED_PROPER_LIST );

        FirstClassObject[] evaluatedArgs = _emptyArgArray;

        if ( argumentList != Cons.NIL )
        {
            evaluatedArgs = argumentList.asArray();
            for ( int i = evaluatedArgs.length-1 ; i >= 0 ; i-- )
                evaluatedArgs[i] = evaluate( evaluatedArgs[i], parent );
        }

        if ( _closure != null )
            return super.activate( _closure, Cons.create( evaluatedArgs ) );

        return super.activate( parent, Cons.create( evaluatedArgs ) );
    }
}
