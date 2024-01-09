/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.lang.reflect.Executable;
import java.text.MessageFormat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.smack.util.CachedHolder;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.ErrorMessages;

/**
 * An exception to be thrown at run-time of a Scheme program.
 *
 * The Exception message is set by the constructor to either
 * <p>
 *  * a Code element in stringified form. In this case the
 *  Code-member is the the passed code.
 * <p>
 *  * a string passed into the constructor. The code element
 *  is then set to {@link Code#ERROR}.
 * <p>
 * Message generation:
 * <p>
 * If error arguments are set, the number of error arguments
 * is appended to the message (e.g. MESSAGE_2) and the resulting
 * key is looked up in the error messages property file. If found
 * the message is resolved using MessageFormat#format using the message
 * arguments and returned.  If MessageFormat#format throws an exception
 * this is not catched.
 * <p>
 *
 * If the above did not result in a message, then a second lookup
 * into the error message property file is performed using the
 * plain message (e.g. MESSAGE).  If the key exists, then the
 * error arguments are appended as space-delimited string to the
 * resolved value and returned.
 * <p>
 *
 * If the above did not result in a message the plain error message
 * is used and the arror arguments are appended as space-delimited
 * strings and returned.
 *
 * @author Michael Binz
 */
@SuppressWarnings("serial")
public class RuntimeX
    extends Exception
{
    /**
     * The message code.
     */
    private final Code _code;

    /**
     * A reference to the arguments to be formatted into the error message.
     * This is never {@code null}.
     */
    private final Object[] _errorArguments;

    /**
     * The name of the operation where the exception occurred.
     */
    private Symbol _operationName = null;

    /**
     * Predefined error codes.
     */
    public enum Code
    {
        INTERNAL_ERROR,
        NOT_IMPLEMENTED,
        SYMBOL_NOT_DEFINED,
        SYMBOL_NOT_ASSIGNABLE,
        TOO_MANY_SUBEXPRESSIONS,
        SYNTAX_ERROR,
        DEFINE_ERROR,
        EXPECTED_PROPER_LIST,
        INDEX_OUT_OF_BOUNDS,
        CALLED_NON_PROCEDURAL,
        INVALID_ASSOC_LIST,
        CAR_FAILED,
        TYPE_ERROR,
        NOT_ENOUGH_ARGUMENTS,
        TOO_MANY_ARGUMENTS,
        WRONG_NUMBER_OF_ARGUMENTS,
        REQUIRES_EQUIVALENT_CONS_LEN,
        BAD_BINDING,
        BAD_CLAUSE,
        DIVISION_BY_ZERO,
        PORT_CLOSED,
        EXPECTED_INPUT_PORT,
        EXPECTED_OUTPUT_PORT,
        IO_ERROR,
        DUPLICATE_FORMAL,
        INVALID_FORMALS,
        CLASS_NOT_FOUND,
        FIELD_NOT_FOUND,
        METHOD_NOT_FOUND,
        ILLEGAL_ACCESS,
        INVOCATION_EXCEPTION,
        CANNOT_ACCESS_INSTANCE,
        CREATION_FAILED,
        ILLEGAL_ARGUMENT,
        SCAN_UNBALANCED_QUOTE,
        SCAN_UNEXPECTED_CHAR,
        ERROR,
        PARSE_EXPECTED,
        PARSE_UNEXPECTED_EOF,
        PARSE_UNEXPECTED,
        INTERRUPTED,
        CANNOT_MODIFY_CONSTANT,
        NO_PROXY,
        PROXY_CANNOT_INSTANTIATE,
        TEST_FAILED,
        ONLY_IN_QUASIQUOTE_CONTEXT,
        RADIX_NOT_SUPPORTED,
        DUPLICATE_ELEMENT,
        EXPECTED_BINARY_PORT,
        EXPECTED_TEXTUAL_PORT,
        SCAN_UNBALANCED_COMMENT,
        RANGE_EXCEEDED,
        RAISE,
        NOT_CONTINUABLE;

        public int id()
        {
            return ordinal() -1;
        }
    }

    private static final CachedHolder<Map<String,Code>>
    _nameToCode =
        new CachedHolder<Map<String,Code>>(
            () -> {
                var result = new HashMap<String, Code>();

                for ( var c : Code.values() )
                    result.put( c.toString(), c );
                return Collections.unmodifiableMap( result );
            }
    );

    private final CachedHolder<String> _message =
            new CachedHolder<String>( this::makeMessage );

    /**
     * Creates a Scream exception.
     *
     * @param msg The message that will be used as a key into Scream's error
     * message properties. This must be neither null nor a blank string.
     * If this is not a stringified element of the Code-enumeration, the
     * Code-member is set to {@link Code#ERROR}.
     * @param args The arguments to be formatted into the message.
     * @throws IllegalArgumentException In case the passed message was blank.
     * @throws NullPointerException In case the passed message was
     * @{code null}.
     */
    public RuntimeX( String msg, Object ... args )
    {
        super( Objects.requireNonNull( msg ) );

        if ( msg.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        _code =
                getCode( msg );
        _errorArguments =
                args == null ?
                    new String[0] :
                    args;
    }

    public RuntimeX( Code c, Object ... args )
    {
        super( Objects.requireNonNull( c ).toString() );
        _code = c;
        _errorArguments =
                args == null ?
                    new String[0] :
                    args;
    }

    public RuntimeX addCause( Throwable cause )
    {
        super.initCause( cause );
        return this;
    }

    /**
     * Get an error code for the passed name.
     *
     * @param name An error name.
     * @return If the name does not match one of the well-defined codes a default code of
     * Code.ERROR is returned, otherwise the respective error code.
     */
    private static Code getCode( String name )
    {
        var result = _nameToCode.get().get( name );

        if ( result != null )
            return result;

        return Code.ERROR;
    }

    /**
     * Sets the name of the throwing operation.  Note that this is only set once
     * and locked.  Further calls to set do <i>not</i> change the operation name
     * set by the first call to this method.
     *
     * @param operation The name of the throwing operation.
     * @see ScreamException#getOperationName
     */
    public RuntimeX setOperationName( Symbol operation )
    {
        if ( _operationName == null )
            _operationName = operation;

        return this;
    }

    /**
     * @return The symbolic name of the operation that reported the exception.
     * @see ScreamException#setOperationName
     */
    public Symbol getOperationName()
    {
        return _operationName;
    }

    /**
     * @return Never {@code null}.
     */
    public Object[] getArguments()
    {
        return _errorArguments;
    }
    public Object getArgument( int idx )
    {
        return _errorArguments[idx];
    }

    /**
     * @return The numeric error id.
     */
    public int getId()
    {
        return _code.id();
    }

    /**
     * @return The exception's code.
     */
    public Code getCode()
    {
        return _code;
    }

    /**
     * @return This exception's message.
     * See {@link RuntimeX} class documentation.
     */
    @Override
    public String getMessage()
    {
        return _message.get();
    }

    /**
     * @return A message. See {@link RuntimeX} class documentation.
     */
    private String makeMessage()
    {
        // Get the original message.  Note that this can neither be null nor
        // the empty string.  See invariant in constructor.
        String messageId = super.getMessage();

        StringBuilder result = new StringBuilder().
                append( getId() ).
                append( " : " );

        if ( getOperationName() != null )
            result.append( getOperationName() ).append( " : " );

        final var messageKey = _errorArguments.length > 0 ?
                messageId + "_" + _errorArguments.length :
                messageId;

        if ( ErrorMessages.map.containsKey( messageKey ) )
        {
            return result.toString() + MessageFormat.format(
                    ErrorMessages.map.get( messageKey ),
                    _errorArguments );
        }

        result.append( messageId );

        for ( var c : _errorArguments )
        {
            result.append( " " );
            result.append( c );
        }

        return result.toString();
    }

    /**
     * Scream specific {@code (error ...)} procedure.  Interrupts the
     * current computation with a runtime error.
     */
    static private Procedure errorProcedure( Environment e )
    {
        return new Procedure( "error" )
        {
            @Override
            protected Thunk __executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, Integer.MAX_VALUE, args );
                checkArgument( 1, SchemeString.class, args.listRef( 0 ) );

                String message = createReadable( args.listRef( 0 ) );

                Object[] arguments = new Object[ (int)(args.length() -1) ];
                for ( int i = 1 ; i < (int)(args.length()) ; i++ )
                    arguments[i-1] = createReadable( args.listRef( i ) );

                RuntimeX result = new RuntimeX( message, arguments );

                result.setOperationName( e.getName() );

                throw result;
            }

            /**
             * Makes a human readable string from a FirstClassObject.  That means for
             * a scheme string that the double quotes are removed -- gnah instead
             * of "gnah" -- and that for all other cases the FCO.toString is called.
             */
            private String createReadable( FirstClassObject o )
            {
                String result;

                if ( o instanceof SchemeString )
                    result = ((SchemeString)o).getValue();
                else
                    result = FirstClassObject.toString( o );

                return result;
            }
        }.setClosure( e );
    }

    /**
     * This is just a standard extendTopLevelEnvironment method as on other
     * Scream classes.
     *
     * @param tle The top-level environment to be extended.
     * @throws RuntimeX
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
            throws RuntimeX
    {
        tle.setPrimitive( errorProcedure( tle ) );
        return tle;
    }

    //    INTERNAL_ERROR = \
    //    -1 : Internal error.
    public static RuntimeX mInternalError()
    {
        return new RuntimeX(
                Code.INTERNAL_ERROR );
    }

    //    INTERNAL_ERROR_1 = \
    //    -1 : Internal error: {0}.
    public static RuntimeX mInternalError( Object msg )
    {
        return new RuntimeX(
                Code.INTERNAL_ERROR,
                Objects.toString( msg ) );
    }

    //    # This functionality is not implemented.
    //    #
    //    NOT_IMPLEMENTED = \
    //    0 : Not implemented.
    public static RuntimeX mNotImplemented()
    {
        return new RuntimeX(
                Code.NOT_IMPLEMENTED );
    }
    public static RuntimeX mNotImplemented( String message )
    {
        return new RuntimeX(
                Code.NOT_IMPLEMENTED, message );
    }

    //    # A symbol is not defined.  The argument should give the symbol name.
    //    #
    //    SYMBOL_NOT_DEFINED_1 = \
    //    1 : Symbol ''{0}'' not defined.
    public static RuntimeX mSymbolNotDefined( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.SYMBOL_NOT_DEFINED, fco );
    }

    //    # Error message is related to the scheme set! special form.  Only bound symbols
    //    # can be assigned using this.  In case the symbol is not bound this error
    //    # message is thrown.
    //    # The argument gives the symbol name that was tried to be assigned.
    //    #
    //    SYMBOL_NOT_ASSIGNABLE_1 = \
    //    2 : Symbol ''{0}'' is not assignable.
    public static RuntimeX mSymbolNotAssignable( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.SYMBOL_NOT_ASSIGNABLE, fco );
    }

    //    TOO_MANY_SUBEXPRESSIONS = \
    //    3 : Expression has too many subexpressions.
    public static RuntimeX mTooManySubexpressions()
    {
        return new RuntimeX(
                Code.TOO_MANY_SUBEXPRESSIONS );
    }

    //    TOO_MANY_SUBEXPRESSIONS_1 = \
    //    3 : Expression has too many subexpressions: {0}.
    public static RuntimeX mTooManySubexpressions( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.TOO_MANY_SUBEXPRESSIONS,
                fco );
    }

    //    # A general error message used for syntax errors.  The argument should describe
    //    # the context of the error.
    //    #
    //    SYNTAX_ERROR = \
    //    4 : Syntax error.
    public static RuntimeX mSyntaxError()
    {
        return new RuntimeX(
                Code.SYNTAX_ERROR );
    }

    //    SYNTAX_ERROR_1 = \
    //    4 : Syntax error in {0}
    public static RuntimeX mSyntaxError( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.SYNTAX_ERROR,
                fco );
    }

    //    DEFINE_ERROR = \
    //    5 : Invalid identifier for define.
    public static RuntimeX mDefineError()
    {
        return new RuntimeX(
                Code.DEFINE_ERROR );
    }

    //    EXPECTED_PROPER_LIST = \
    //    6 : Expected proper list.
    public static RuntimeX mExpectedProperList()
    {
        return new RuntimeX(
                Code.EXPECTED_PROPER_LIST );
    }

    //    EXPECTED_PROPER_LIST_1 = \
    //    6 : Expected proper list.  Received {0}
    public static RuntimeX mExpectedProperList(
            FirstClassObject actual )
    {
        return new RuntimeX(
                Code.EXPECTED_PROPER_LIST,
                actual );
    }

    //    INDEX_OUT_OF_BOUNDS_1 = \
    //    7 : Index out of bounds: {0}
    public static RuntimeX mIndexOutOfBounds( long actual )
    {
        return new RuntimeX(
                Code.INDEX_OUT_OF_BOUNDS,
                actual );
    }

    //    CALLED_NON_PROCEDURAL_1 = \
    //    8 : Attempt to call non-procedural object {0}.
    public static RuntimeX mCalledNonProcedural( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.CALLED_NON_PROCEDURAL,
                FirstClassObject.toString( fco ) );
    }

    //    INVALID_ASSOC_LIST_1 = \
    //    9 : Invalid association list: {0}
    public static RuntimeX mInvalidAssocList( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.INVALID_ASSOC_LIST,
                fco );
    }

    //    CAR_FAILED_1 = \
    //    10: Can't get car for {0}.
    public static RuntimeX mCarFailed( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.CAR_FAILED,
                fco );
    }

    //    # 0: Name of expected type
    //    # 1: Name of actual type
    //    # 2: Optional: Position of wrong parameter in a parameter list.
    //    #
    //    TYPE_ERROR_2 = \
    //    11 : Argument has wrong type.  Expected {0} but found {1}.
    public static  <T extends FirstClassObject>
    RuntimeX mTypeError( Class<T> expected, FirstClassObject fco )
    {
        String msg = Cons.NIL == fco ?
                FirstClassObject.toString( fco ) :
                fco.typename() + "=" + fco.toString();

        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                msg );
    }

    //    TYPE_ERROR_3 = \
    //    11 : Argument {2} has wrong type.  Expected {0} but found {1}.
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError( Class<T1> expected, Class<T2> found, int argumentIdx )
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.typename( found ),
                argumentIdx );
    }

    //    NOT_ENOUGH_ARGUMENTS_1 = \
    //    12 : Wrong number of arguments.  Expected at least {0}.
    public static RuntimeX mNotEnoughArguments( long minExpected )
    {
        return new RuntimeX(
                Code.NOT_ENOUGH_ARGUMENTS,
                minExpected );
    }

    //    NOT_ENOUGH_ARGUMENTS_2 = \
    //    12 : Wrong number of arguments.  Expected at least {0} but received {1}.
    public static RuntimeX mNotEnoughArguments( long minExpected, long received )
    {
        return new RuntimeX(
                Code.NOT_ENOUGH_ARGUMENTS,
                minExpected,
                received );
    }

    //    TOO_MANY_ARGUMENTS_1 = \
    //    13 : Wrong number of arguments.  Expected at most {0}.
    public static RuntimeX mTooManyArguments( long maxExpected )
    {
        return new RuntimeX(
                Code.TOO_MANY_ARGUMENTS,
                maxExpected );
    }

    //    TOO_MANY_ARGUMENTS_2 = \
    //    13 : Wrong number of arguments.  Expected at most {0} but received {1}.
    public static RuntimeX mTooManyArguments( long maxExpected, long received )
    {
        return new RuntimeX(
                Code.TOO_MANY_ARGUMENTS,
                maxExpected,
                received );
    }

    //    # arg 0: Number of received parameters
    public static RuntimeX mWrongNumberOfArguments( long received )
    {
        return new RuntimeX(
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                received );
    }

    //    # arg 0: Number of expected parameters
    //    # arg 1: Number of received parameters
    //    #
    //    WRONG_NUMBER_OF_ARGUMENTS_2 = \
    //    14 : Wrong number of arguments.  Expected {0} but received {1}.
    public static RuntimeX mWrongNumberOfArguments( long expected, long received )
    {
        return new RuntimeX(
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                expected,
                received );
    }

    //    # Procedure (map) specific.  First passed argument has to be a procedure, all
    //    # remaining arguments have to be lists of the same length.
    //    #
    //    REQUIRES_EQUIVALENT_CONS_LEN = \
    //    15 : All passed lists have to have the same length.
    public static RuntimeX mRequiresEqivalentConsLength()
    {
        return new RuntimeX(
                Code.REQUIRES_EQUIVALENT_CONS_LEN );
    }

    //    # Used in a number of syntax implementations.
    //    #
    //    # arg 0: The name of the syntax.
    //    # arg 1: The wrong binding
    //    #
    //    BAD_BINDING_2 = \
    //    16 : Bad binding in {0} syntax: {1}
    public static RuntimeX mBadBinding( FirstClassObject syntax, FirstClassObject binding )
    {
        return new RuntimeX(
                Code.BAD_BINDING,
                syntax,
                binding );
    }

    //    BAD_CLAUSE_1 = \
    //    17 : Bad clause: {0}
    public static RuntimeX mBadClause( FirstClassObject clause )
    {
        return new RuntimeX(
                Code.BAD_CLAUSE,
                clause );
    }

    //    DIVISION_BY_ZERO = \
    //    18 : Division by zero.
    public static RuntimeX mDivisionByZero()
    {
        return new RuntimeX(
                Code.DIVISION_BY_ZERO );
    }

    //    # It has been tried to write to or read from a closed port.
    //    #
    //    PORT_CLOSED = \
    //    19 : Port is closed.
    public static RuntimeX mPortClosed()
    {
        return new RuntimeX(
                Code.PORT_CLOSED );
    }

    //    # Used in the port related input procedures.
    //    #
    //    EXPECTED_INPUT_PORT = \
    //    20 : Tried to read from output port.
    public static RuntimeX mExpectedInputPort()
    {
        return new RuntimeX(
                Code.EXPECTED_INPUT_PORT );
    }

    //    # Used in the port related output procedures.
    //    #
    //    EXPECTED_OUTPUT_PORT = \
    //    21 : Tried to write on input port.
    public static RuntimeX mExpectedOutputPort()
    {
        return new RuntimeX(
                Code.EXPECTED_OUTPUT_PORT );
    }

    //    # An I/O error has occured.
    //    #
    //    # arg 0:  The system message.
    //    #
    //    IO_ERROR_1 = \
    //    22 : Input/Output operation failed: {0}
    public static RuntimeX mIoError( IOException e )
    {
        return new RuntimeX(
                Code.IO_ERROR,
                e.getMessage() );
    }

    //    DUPLICATE_FORMAL_1 = \
    //    23 : Duplicate formal argument name: {0}
    public static RuntimeX mDuplicateFormal( FirstClassObject formal )
    {
        return new RuntimeX(
                Code.DUPLICATE_FORMAL,
                formal );
    }

    //    INVALID_FORMALS_1 = \
    //    24 : Invalid formal argument list: {0}
    public static RuntimeX mInvalidFormals( FirstClassObject formals )
    {
        return new RuntimeX(
                Code.INVALID_FORMALS,
                formals );
    }

    //    CLASS_NOT_FOUND_1 = \
    //    25 : Class {0} not found.
    public static RuntimeX mClassNotFound( String name )
    {
        return new RuntimeX(
                Code.CLASS_NOT_FOUND,
                name );
    }

    //    FIELD_NOT_FOUND_1 = \
    //    26 : Field not found: {0}
    public static RuntimeX mFieldNotFound( String name )
    {
        return new RuntimeX(
                Code.FIELD_NOT_FOUND,
                name );
    }

    //    METHOD_NOT_FOUND_1 = \
    //    27 : Method not found: {0}
    public static RuntimeX mMethodNotFound( String name )
    {
        return new RuntimeX(
                Code.METHOD_NOT_FOUND,
                name );
    }
    //    METHOD_NOT_FOUND_1 = \
    //    27 : Method not found: {0}
    public static RuntimeX mMethodNotFound( String name, Cons arguments )
    {
        return new RuntimeX(
                Code.METHOD_NOT_FOUND,
                name + FirstClassObject.toString( arguments ) );
    }

    //    ILLEGAL_ACCESS_1 = \
    //    28 : No access to {0}.
    public static RuntimeX mIllegalAccess( String name )
    {
        return new RuntimeX(
                Code.ILLEGAL_ACCESS,
                name );
    }

    //    # A method invoked by reflection threw an exception.  Note that this exception
    //    # always has to be different from a RuntimeX, this case has to be handled.
    //    #
    //    INVOCATION_EXCEPTION_2 = \
    //    29 : Invoked method {0} threw exception {1}.  See logfile for full exception.
    public static RuntimeX mInvocationException( Executable method, Throwable e )
    {
        return new RuntimeX(
                Code.INVOCATION_EXCEPTION,
                method,
                e.toString() );
    }

    //    # It has been tried to access instance information on a class.
    //    #
    //    CANT_ACCESS_INSTANCE = \
    //    30 : Tried to access instance information on class object
    public static RuntimeX mCannotAccessInstance()
    {
        return new RuntimeX(
                Code.CANNOT_ACCESS_INSTANCE );
    }

    //    # arg 0: The class name that was tried to instantiate.
    //    #
    //    CREATION_FAILED = \
    //    31 : Can't create an instance of {0}.
    public static RuntimeX mCreationFailed( String name )
    {
        return new RuntimeX(
                Code.CREATION_FAILED,
                name );
    }

    //    # An illegal argument was passed into a reflective invocation of a method.
    //    # Used in SchemeObject.
    //    #
    //    ILLEGAL_ARGUMENT_1 = \
    //    32 : Illegal argument for {0}.
    public static RuntimeX mIllegalArgument( String name )
    {
        return new RuntimeX(
                Code.ILLEGAL_ARGUMENT,
                name );
    }

    //    # An unbalanced quote has been found.
    //    #
    //    SCAN_UNBALANCED_QUOTE = \
    //    33 : Unbalanced quote.
    public static RuntimeX mScanUnbalancedQuote()
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_QUOTE );
    }

    //    SCAN_UNBALANCED_QUOTE_2 = \
    //    33 : Unbalanced quote found at line {0}, column {1}.
    public static RuntimeX mScanUnbalancedQuote( int line, int column )
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_QUOTE,
                line,
                column );
    }

    //    SCAN_UNEXPECTED_CHAR_3 = \
    //    34 : Unexpected character ''{2}'' found at line {0}, column {1}.
    public static RuntimeX mScanUnexpectedCharacter( int line, int col, String character )
    {
        return new RuntimeX(
                Code.SCAN_UNEXPECTED_CHAR,
                line,
                col,
                character );
    }

    //    ERROR = \
    //    Error.
    public static RuntimeX mError()
    {
        return new RuntimeX(
                Code.ERROR );
    }

    //    # The parser didn't receive an expected token.
    //    #
    //    # arg 0: The name of the expected token.
    //    #
    //    PARSE_EXPECTED_1 = \
    //    36 : Expected {0}.
    public static RuntimeX mParseExpected( Tk token )
    {
        return new RuntimeX(
                Code.PARSE_EXPECTED,
                token );
    }

    //    # The parser found a premature end of file.
    //    #
    //    PARSE_UNEXPECTED_EOF = \
    //    37 : Unexpected end of file.
    public static RuntimeX mParseUnexpectedEof()
    {
        return new RuntimeX(
                Code.PARSE_UNEXPECTED_EOF );
    }

    //    # The parser found a token that was not expected.
    //    #
    //    PARSE_UNEXPECTED_1 = \
    //    38 : Unexpected {0}.
    public static RuntimeX mParseUnexpected( Token token )
    {
        return new RuntimeX(
                Code.PARSE_UNEXPECTED,
                token );
    }

    //    INTERRUPTED = \
    //    39 : Computation interrupted.
    public static RuntimeX mInterrupted()
    {
        return new RuntimeX(
                Code.INTERRUPTED );
    }

    //    CANT_MODIFY_CONSTANT = \
    //    40 : Tried to modify constant.
    public static RuntimeX mCannotModifyConstant()
    {
        return new RuntimeX(
                Code.CANNOT_MODIFY_CONSTANT );
    }

    //    CANT_MODIFY_CONSTANT_1 = \
    //    40 : Tried to modify constant: {0}
    public static RuntimeX mCannotModifyConstant( FirstClassObject constant )
    {
        return new RuntimeX(
                Code.CANNOT_MODIFY_CONSTANT,
                constant );
    }

    //    # arg 0: The name of the class that has been tried to instantiate as a proxy.
    //    #
    //    NO_PROXY_1 = \
    //    41 : Not a proxy interface: {0}
    public static RuntimeX mNoProxy( String name )
    {
        return new RuntimeX(
                Code.NO_PROXY,
                name );
    }

    //    PROXY_CANT_INSTANCIATE = \
    //    42 : Can't instantiate proxy.
    public static RuntimeX mProxyCannotInstantiate()
    {
        return new RuntimeX(
                Code.PROXY_CANNOT_INSTANTIATE );
    }
    //    PROXY_CANT_INSTANCIATE_1 = \
    //    42 : Can't instantiate proxy: {0}
    public static RuntimeX mProxyCannotInstantiate( String name )
    {
        return new RuntimeX(
                Code.PROXY_CANNOT_INSTANTIATE,
                name );
    }

    //    # Will be used in automatic regression testing.
    //    #
    //    TEST_FAILED_2 = \
    //    43 : Test {0}#{1} failed.
    public static RuntimeX mTestFailed( String group, String name )
    {
        return new RuntimeX(
                Code.TEST_FAILED,
                group,
                name );
    }

    //    ONLY_IN_QUASIQUOTE_CONTEXT = \
    //    44 : Only applicable in quasiquote template.
    public static RuntimeX mOnlyInQuasiquoteContext()
    {
        return new RuntimeX(
                Code.ONLY_IN_QUASIQUOTE_CONTEXT );
    }

    //    RADIX_NOT_SUPPORTED_2 = \
    //    45 : Radix {0} is not supported.  Maximum radix is {1}.
    public static RuntimeX mRadixNotSupported( int radix, int maxRadix )
    {
        return new RuntimeX(
                Code.RADIX_NOT_SUPPORTED,
                radix,
                maxRadix );
    }

    //    # A list contained a duplicate element.  Used in case-syntax.
    //    #
    //    DUPLICATE_ELEMENT_1 = \
    //    46 : Duplicate element : {0}
    public static RuntimeX mDuplicateElement( FirstClassObject duplicate )
    {
        return new RuntimeX(
                Code.DUPLICATE_ELEMENT,
                duplicate );
    }

    // EXPECTED_BINARY_PORT_1 = \
    // 47 : Expected binary port, got {0}.
    public static RuntimeX mExpectedBinaryPort( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.EXPECTED_BINARY_PORT,
                fco );
    }

    // EXPECTED_TEXTUAL_PORT_1 = \
    // 47 : Expected textual port, got {0}.
    public static RuntimeX mExpectedTextualPort( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.EXPECTED_TEXTUAL_PORT,
                fco );
    }

    // SCAN_UNBALANCED_COMMENT_2 = \
    // 49 : Unbalanced comment found at line {0}, column {1}.
    public static RuntimeX mScanUnbalancedComment( int line, int column )
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_COMMENT,
                line,
                column );
    }

    // RANGE_EXCEEDED_1 = \
    // 50 : Range exceeded. Actual {0}, expected {1}.
    public static RuntimeX mRangeExceeded( FirstClassObject fco, String rangeDescription )
    {
        return new RuntimeX(
                Code.RANGE_EXCEEDED,
                fco,
                rangeDescription );
    }

    // TODO
    // RAISE_1 = \
    // 51 : Range exceeded. Actual {0}, expected {1}.
    public static RuntimeX mRaise( Environment environment, FirstClassObject fco )
    {
        return new RuntimeX(
                Code.RAISE,
                environment,
                fco );
    }

    // TODO
    // RAISE_1 = \
    // 51 : Range exceeded. Actual {0}, expected {1}.
    public static RuntimeX mNotContinuable()
    {
        return new RuntimeX(
                Code.NOT_CONTINUABLE );
    }
}
