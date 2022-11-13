/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import java.util.HashSet;

import de.michab.scream.Lambda.L;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.pops.Continuation;
import de.michab.scream.pops.Continuation.Cont;
import de.michab.scream.pops.Continuation.Thunk;

/**
 * Represents an abstract operation.  Is the base class for macros (syntaxes in
 * Scheme parlance) and procedures.
 *
 * @author Michael Binz
 */
public class Operation
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = Operation.class.getSimpleName();

    /**
     * The array representation of an empty argument list.
     */
    static final protected FirstClassObject[] _emptyArgArray =
            new FirstClassObject[0];

    /**
     * A default name used in the no-argument constructor.
     */
    static protected final Symbol DEFAULT_NAME =
            Symbol.createObject( "anonymous" );

    /**
     * A name for this Operation.
     */
    private Symbol _name = null;

    /**
     * This operation's body.
     */
    protected Cons _body = Cons.NIL;

    /**
     * The formal argument list for this operation.
     */
    protected Cons _formalArguments = Cons.NIL;

    /**
     * The symbol being bound to the rest of the argument list.  This symbol can
     * be used to access the dynamic arguments of an operation invocation.
     */
    protected FirstClassObject _rest = Cons.NIL;

    /**
     * Creates a named operation.  Main purpose is for operation-derived objects
     * implemented in Java.
     *
     * @param name A symbolic name for the new operation.  Used for error
     *             reporting.
     */
    protected Operation( Symbol name )
    {
        commonInit( name );
    }
    protected Operation( String name )
    {
        this( Symbol.createObject( name ) );
    }

    /**
     * Default constructor.  Creates an operation with a default name. Note that
     * operations should always explicitly receive a name to support error
     * reporting.  Use the constructor for creating named operations instead.
     *
     * @see Operation#DEFAULT_NAME
     */
    protected Operation()
    {
        this( DEFAULT_NAME );
    }

    /**
     * Used for scheme defined @{code Operation} objects.
     *
     * @param formalArguments The operation's formal arguments as specified in
     *        the source code.
     * @param body The operation's body.
     * @throws RuntimeX In case the definition is syntactically wrong.
     */
    protected Operation( FirstClassObject
            formalArguments,
            Cons body,
            Environment compileEnv )
                    throws RuntimeX
    {
        // Set the body and the argument list to constant.  This is not absolutely
        // necessary if everything is done right.  But of we do something wrong
        // this results in early detection.
//        setConstant( formalArguments, true );
//        setConstant( body, true );
        // Replace the argument list by a clone that gets modified later-on.
        formalArguments = copy( formalArguments );

        commonInit( DEFAULT_NAME );

        // Not very much to check for.
        FirstClassObject[] fcoa = body.asArray();
        for ( int i = 0 ; i < fcoa.length ; i++ )
            fcoa[i] = FirstClassObject.compile( fcoa[i], compileEnv  );
        _body = Cons.create( fcoa );

        // If the formal argument list is empty ...
        if ( Cons.NIL == formalArguments )
            // ...we are ready.
            return;

        // If the formal arguments are only a symbol...
        if ( formalArguments instanceof Symbol )
        {
            // ...this means variable number of arguments.
            _rest = formalArguments;
            // Ready.
            return;
        }

        // The formal arguments must be a list.
        if ( ! (formalArguments instanceof Cons) )
            throw new RuntimeX( Code.SYNTAX_ERROR, formalArguments );

        // Remember the formal argument list.
        Cons fac = _formalArguments = (Cons)formalArguments;

        // The formal argument list must be a list of unique symbols.
        java.util.Set<String> unifier = new HashSet<String>();

        // TODO rethink this loop ... especially the break conditions.
        while ( fac != Cons.NIL )
        {
            // Check the car.
            FirstClassObject car = fac.getCar();

            if ( car instanceof Symbol )
            {
                String symbolName = car.toString();

                // Set.add() returns false if the element is already contained.
                if ( false == unifier.add( symbolName ) )
                    throw new RuntimeX( Code.DUPLICATE_FORMAL, car );

                // This cell was fine.
            }
            else
                throw new RuntimeX( Code.INVALID_FORMALS, car );

            // Check the cdr.
            FirstClassObject cdr = fac.getCdr();

            if ( cdr == Cons.NIL )
                break;

            else if ( cdr instanceof Cons )
                // Standard case.
                fac = (Cons)cdr;

            else if ( cdr instanceof Symbol )
            {
                // A 'rest' symbol was defined.
                // Set.add() returns false if the element is already contained.
                if ( false == unifier.add( cdr.toString() ) )
                    throw new RuntimeX( Code.DUPLICATE_FORMAL, cdr );
                else
                {
                    // Terminate the formal argument list...
                    fac.setCdr( Cons.NIL );
                    // ...and remember the rest symbol.
                    _rest = cdr;
                }
                // Done.  Break out of the loop.
                break;
            }
            else
                throw new RuntimeX( Code.INVALID_FORMALS, cdr );
        }
    }

    /**
     * Common initialization code.  Used from the constructors.
     */
    private void commonInit( Symbol name )
    {
        _name = name;
        setConstant( false );
    }

    /**
     * @return {@code true} if this is a variadic operation. (Accepts a
     * variable number of arguments.)
     */
    public boolean isVariadic()
    {
        return _rest != Cons.NIL;
    }

    public long argumentCount()
            throws RuntimeX
    {
        return _formalArguments == null ?
                0 :
                _formalArguments.length();
    }

    /**
     * Execute the operation in a given environment and based on the passed
     * parameters.  This default implementation just forwards the call to
     * <code>activate( Environment, FirstClassObject[] )</code>.
     *
     * @param e The environment to use for the current activation.
     * @param argumentList The list of arguments passed into the current
     *        activation.
     * @return The result of the activation.
     * @throws RuntimeX In case the activation failed.
     */
    public FirstClassObject activate( Environment e, Cons argumentList )
            throws RuntimeX
    {
        return activate( e, Cons.asArray( argumentList ) );
    }

    protected Thunk _bind( Environment e, Cons argNames, Cons argValues, Cont<Environment> c )
        throws RuntimeX
    {
        if ( argNames != Cons.NIL && argValues == Cons.NIL)
            // Cannot happen since we check in _activate.
            throw new InternalError( "NotEnough" );
        if ( argNames == Cons.NIL && argValues == Cons.NIL )
            return c.accept( e );
        if ( argNames == Cons.NIL && _rest != Cons.NIL )
        {
            e.define( (Symbol)_rest, argValues );
            return c.accept( e );
        }
        if ( argNames == Cons.NIL && _rest == Cons.NIL )
        {
            // Cannot happen since we check in _activate.
            throw new InternalError( "TooMany" );
        }

        Symbol name =
                (Symbol)argNames.getCar();
        FirstClassObject value =
                argValues.getCar();
        e.define( name, value );

        return () -> _bind( e, (Cons)argNames.getCdr(), (Cons)argValues.getCdr(), c );
    }

    public Thunk _activate( Environment e, Cons args, Cont<FirstClassObject> c )
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
                (s)->Continuation._x_begin( s, _body, c ) );
    }

    /**
     * Execute the operation in a given environment and based on the passed
     * parameters.  This default implementation executes an
     * <code>Operation</code> that is specified in Scheme as opposed to
     * implemented in Java.  This method represents the final step in a chain of
     * calls that can be overridden by Java operation implementations.  If none
     * of these entries is overridden the activation ends up here.
     *
     * @param e The environment to use for the current activation.
     * @param argumentList The list of arguments passed into the current
     *        activation.
     * @return The result of the activation.
     * @throws RuntimeX In case the activation failed.
     */
    protected FirstClassObject activate( Environment e,
            FirstClassObject[] argumentList )
                    throws RuntimeX
    {
        // We're nearly ready to execute -- so let's extend the environment.
        Environment executionEnvironment = e.extend( getName() );

        Cons formalArg = _formalArguments;

        // Initialise the 'rest' variable in the current environment.
        if ( _rest != Cons.NIL )
            executionEnvironment.define( (Symbol)_rest, Cons.NIL );

        for ( int i = 0 ; i < argumentList.length ; i++ )
        {
            // If we haven't reached the end of our formal argument list...
            if ( formalArg != Cons.NIL )
            {
                // ...bind the argument.
                executionEnvironment.define( (Symbol)formalArg.getCar(), argumentList[i] );
                formalArg = (Cons)formalArg.getCdr();
            }
            else if ( _rest != Cons.NIL )
            {
                // ...bind it.
                executionEnvironment.define( (Symbol)_rest, Cons.create( argumentList, i ) );
                break;
            }
            else
            {
                int numberOfFormals = 0;

                if ( _formalArguments != null )
                    numberOfFormals = (int)_formalArguments.length();

                throw new RuntimeX( Code.WRONG_NUMBER_OF_ARGUMENTS,
                        "" + numberOfFormals,
                        "" + argumentList.length
                        );
            }
        }

        // Now check if there are more formal args.
        if ( Cons.NIL != formalArg )
            throw new RuntimeX( Code.WRONG_NUMBER_OF_ARGUMENTS,
                    "" + _formalArguments.length(),
                    "" + argumentList.length
                    );

        // What is left to be done is the actual evaluation of the body.
        Cons expressionQueue = _body;
        FirstClassObject result = Cons.NIL;

        do
        {
            FirstClassObject currentExpr = expressionQueue.getCar();
            expressionQueue = (Cons)expressionQueue.getCdr();

            // If this is the last entry in the expression queue...
            if ( expressionQueue == Cons.NIL )
                // ...then this is evaluated in a trailing context.
                result = evaluateTrailingContext( currentExpr, executionEnvironment );
            else
                result = evaluate( currentExpr, executionEnvironment );

        } while ( expressionQueue != Cons.NIL );

        return result;
    }

    /**
     * Set this operation's symbolic name.  Default name is 'anonymous'.
     *
     * @param name The symbolic name for the operation.
     */
    public Operation setName( Symbol name )
    {
        _name = name;
        return this;
    }

    /**
     * Get this <code>Operation</code>'s symbolic name.
     *
     * @return The operation's symbolic name.  If no name has been set then this
     *         will be 'anonymous'.
     * @see Operation#setName
     */
    public Symbol getName()
    {
        return _name;
    }

    /**
     * Checks if the length of the actual argument list is the length we expect.
     *
     * @param expected The expected number of arguments.
     * @param received The array of arguments received.
     * @throws RuntimeX If the number of arguments was wrong.
     */
    static protected void checkArgumentCount(
            int expected,
            FirstClassObject[] received )
                    throws RuntimeX
    {
        if ( expected != received.length )
            throw new RuntimeX( Code.WRONG_NUMBER_OF_ARGUMENTS,
                    "" + expected,
                    "" + received.length
                    );
    }

    static protected void checkArgumentCount(
            int expected,
            Cons received )
                    throws RuntimeX
    {
        checkArgumentCount(
                expected,
                Cons.asArray( received ) );
    }
    static protected long checkArgumentCount(
            int min,
            int max,
            Cons received )
                    throws RuntimeX
    {
        long argumentCount =
                Cons.length( received );

        if ( argumentCount < min )
        {
            throw new RuntimeX( Code.NOT_ENOUGH_ARGUMENTS,
                    min,
                    argumentCount );
        }
        if ( argumentCount > max )
        {
            throw new RuntimeX( Code.TOO_MANY_ARGUMENTS,
                    max,
                    argumentCount );
        }

        return argumentCount;
    }

    /**
     * Checks if the length of the actual argument list is the length we expect.
     *
     * @param expected The expected number of arguments.
     * @param received The array of arguments received.
     * @throws RuntimeX If the number of arguments was wrong.
     */
    protected void checkArgumentCount( Cons received )
                    throws RuntimeX
    {
        var formalCount = _formalArguments.length();
        var receivedCount = received.length();
        var hasRest = _rest != Cons.NIL;

        if ( formalCount == receivedCount )
            return;
        else if ( formalCount < receivedCount && hasRest )
            return;

        throw new RuntimeX( Code.WRONG_NUMBER_OF_ARGUMENTS,
                "" + formalCount,
                "" + receivedCount );
    }

    /**
     * Checks if at least a minimum number of actual arguments were received.
     *
     * @param minimum The minimum number of parameters required.
     * @param received The parameter list received.
     * @throws RuntimeX In case not enough parameters were passed.
     */
    static protected void checkMinimumArgumentCount(
            int minimum,
            FirstClassObject[] received )
                    throws RuntimeX
    {
        if ( minimum > received.length )
            throw new RuntimeX( Code.NOT_ENOUGH_ARGUMENTS,
                    "" + minimum,
                    "" + received.length
                    );
    }

    /**
     * Checks if at most a maximum number of actual arguments were received.
     *
     * @param maximum The maximum number of acceptable parameters.
     * @param received The parameter list received.
     * @throws RuntimeX In case the passed number of parameters exceeds the
     *         maximum.
     */
    static protected void checkMaximumArgumentCount(
            int maximum,
            FirstClassObject[] received )
                    throws RuntimeX
    {
        if ( maximum < received.length )
            throw new RuntimeX( Code.TOO_MANY_ARGUMENTS,
                    "" + maximum,
                    "" + received.length
                    );
    }

    /**
     * Checks if the received argument types are the expected ones.
     *
     * @param position The argument number in the argument list.
     * @param formal The expected type's class.
     * @param received The actual object received.
     * @throws RuntimeX In case the wrong type was passed.
     */
    static final protected void checkArgument(
            int position,
            Class<?> formal,
            FirstClassObject received )
                    throws RuntimeX
    {
        if ( received == Cons.NIL ||
                ! formal.isAssignableFrom( received.getClass() ) )
            //    if ( received != Cons.NIL &&
            //         ! formal.isAssignableFrom( received.getClass() ) )
        {
            throw new ConversionFailedX( received, formal, position );
        }
    }
    static final protected void checkArgument(
            int position,
            FirstClassObject received,
            Class<?> ... alternatives
            )
                    throws RuntimeX
    {
        if ( received == Cons.NIL )
            // TODO better error message for multiple alternatives.
            throw new ConversionFailedX( received, alternatives[0], position );

        var actual = received.getClass();

        for ( var c : alternatives )
        {
            if ( c.isAssignableFrom( actual ) )
                return;
        }
        // TODO better error message for multiple alternatives.
        throw new ConversionFailedX( received, alternatives[0], position );
    }

    /**
     * Checks if the received argument types are the expected ones.
     *
     * @param formal The array of formal arguments.
     * @param received The array of arguments received.
     * @throws RuntimeX In case the argument lists do not correspond.
     */
    static protected void checkArguments( Class<?>[] formal,
            FirstClassObject[] received )
                    throws RuntimeX
    {
        checkArgumentCount( formal.length, received );

        for ( int i = 0 ; i < formal.length ; i++ )
            checkArgument( i, formal[i], received[i] );
    }

    /**
     * Conversion of Operation instances into Java objects return constantly
     * <code>null</code>.
     *
     * @return Always <code>null</code>.
     */
    @Override
    public Object toJava()
    {
        return null;
    }

    private Thunk _execute( Environment e, Cons args, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        final var ex = e.extend( getName() );

        if ( _rest != Cons.NIL )
            ex.define( (Symbol)_rest, Cons.NIL );

        return () -> _bind(
                ex,
                _formalArguments,
                args,
                (s)->Continuation._x_begin( s, _body, c ) );
    }

    protected Lambda _compile( Environment env, Cons args ) throws RuntimeX
    {
        checkArgumentCount( args );

        L l = (e,c) -> _execute( e, args, c );

        return new Lambda(
                l,
                this.toString() );
    }

    /**
     * Compiles the {@code Operation}.  The passed environment can be
     * explicitly {@code null}.  Compilation will resolve all
     * <code>Syntax</code> in the initial draft implementation.  This default
     * implementation forwards the call to
     * <code>compile( Environment, FirstClassObject[] )</code>.
     *
     * @param e An environment to use for the compilation.  May be
     *        <code>null</code>.
     * @param argList The argument list for compilation.
     * @return The compiled operation.
     * @throws RuntimeX In case of syntax errors.
     */
    public FirstClassObject compile( Environment e, Cons argList )
            throws RuntimeX
    {
        return compile( e, argList.asArray() );
    }

    /**
     * Compiles the <code>Operation</code>.  The passed environment can be
     * explicitly <code>null</code>.  Compilation will resolve all
     * <code>Syntax</code> in the initial draft implementation.  This default
     * implementation returns identity.
     *
     * @param e An environment to use for the compilation.  May be
     *        <code>null</code>.
     * @param argList The argument list as array for compilation.
     * @return The compiled operation.
     * @throws RuntimeX In case of syntax errors.
     */
    public FirstClassObject compile( Environment e, FirstClassObject[] argList )
            throws RuntimeX
    {
        return this;
    }

    /**
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return  String.format(
                "<%s %s>",
                typename(),
                getName() );
    }
}
