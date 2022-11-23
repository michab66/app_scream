/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import de.michab.scream.Lambda.L;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.pops.Continuation;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;

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
    private final Environment _closure;

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

    @Override
    protected Thunk _execute( Environment
            e,
            Cons args,
            Cont<FirstClassObject> c )
                    throws RuntimeX
    {
        checkArgumentCount( args );

        final var ex = e.extend( getName() );

        if ( _rest != Cons.NIL )
            ex.define( (Symbol)_rest, Cons.NIL );

        return () -> _bind(
                ex,
                _formalArguments,
                args,
                s -> Continuation._x_begin( s, _body, c ) );
    }

    /**
     * Evaluates the arguments in the received environment and passes on to
     * {@link #_execute(Environment, Cons, Cont)} for execution of the
     * _body in the procedure's closure.
     *
     * @param e
     * @param args
     * @param c
     * @return
     * @throws RuntimeX
     */
    private Thunk _prepareExecute( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        // Execute in our _closure.
        Cont<Cons> cc = evaluated -> _execute(
                _closure,
                evaluated,
                c );

        // Evaluate the arguments in the environment that we receive.
        return () -> Continuation._x_evalCons(
                e,
                args,
                cc );
    }

    @Override
    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( args );

        L l = (e,c) -> _prepareExecute( e, args, c );

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

        return apply( parent, evaluatedArgs );
    }

    /**
     * This is the public available apply procedure that is used from Scheme
     * implemented methods via reflection.
     *
     * @param evaluatedArgs The list of evaluated arguments to be used for the
     *        <code>apply</code> invocation.
     * @return The result of the procedure application.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject apply( Cons evaluatedArgs )
            throws RuntimeX
    {
        FirstClassObject[] array;

        if ( Cons.NIL == evaluatedArgs )
            array = _emptyArgArray;
        else
            array = evaluatedArgs.asArray();

        // Just map to the array version of this method.
        return apply( array );
    }

    /**
     * Another point to be overridden.  In this case the parent environment
     * is accessible.
     *
     * @param parent The environment to use for this procedure application.
     * @param evaluatedArgs The list of evaluated arguments to be used for the
     *        <code>apply</code> invocation.
     * @return The result of the procedure application.
     * @throws RuntimeX In case of an error.
     */
    @Deprecated
    protected FirstClassObject apply( Environment parent,
            FirstClassObject[] evaluatedArgs )
                    throws RuntimeX
    {
        // Note that this override is only used by the implementations of
        // the %error and evaluate procedures.
        return apply( evaluatedArgs );
    }

    /**
     * If not overridden this method actually executes the Scheme defined
     * procedure.
     *
     * @param evaluatedArgs The list of evaluated arguments to be used for the
     *        <code>apply</code> invocation.
     * @return The result of the procedure application.
     * @throws RuntimeX In case of an error.
     */
    @Deprecated
    protected FirstClassObject apply( FirstClassObject[] evaluatedArgs )
            throws RuntimeX
    {
        return super.activate( _closure, evaluatedArgs );
    }
}
