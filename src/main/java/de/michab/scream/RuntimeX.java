/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
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
import org.smack.util.StringUtil;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Continuation;
import de.michab.scream.fcos.Environment;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.Continuation.Thunk;
import de.michab.scream.util.ErrorMessages;
import de.michab.scream.util.Scut;
import de.michab.scream.util.SourcePosition;

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
 * is used and the error arguments are appended as space-delimited
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
     * A reference to the arguments passed in the constructors.
     * Kept as an array for efficient access.
     *
     * This is never {@code null}.
     */
    private final FirstClassObject[] _irritants;

    /**
     * The name of the operation that caused the exception.
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
        _UNUSED_,
        ONLY_IN_QUASIQUOTE_CONTEXT,
        RADIX_NOT_SUPPORTED,
        DUPLICATE_ELEMENT,
        EXPECTED_BINARY_PORT,
        EXPECTED_TEXTUAL_PORT,
        SCAN_UNBALANCED_COMMENT,
        RANGE_EXCEEDED,
        RAISE,
        NOT_CONTINUABLE;
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

    /**
     * Creates a Scream exception.  Used by the {@code (error ...)} procedure.
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
    RuntimeX( SchemeString msg, FirstClassObject ... args  )
    {
        super( Objects.requireNonNull( msg.getValue() ) );

        var msgValue = msg.getValue();

        if ( msgValue.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        _code =
                getCode( msg.getValue() );
        _irritants =
                Objects.requireNonNull( args );
    }

    RuntimeX( Code c, FirstClassObject ... args )
    {
        super( Objects.requireNonNull( c ).toString() );

        _code =
                c;
        _irritants =
                Objects.requireNonNull( args );
    }

    private static String validateMessage( SchemeString ss )
    {
        var result = Objects.requireNonNull( ss ).getValue();

        if ( result.isBlank() )
            throw new IllegalArgumentException( "Empty message." );

        return result;
    }

    /**
     * Strategic target ctor.
     *
     * @param operation
     * @param msg
     * @param irritants
     */
    public RuntimeX(
            Symbol operation,
            SchemeString msg,
            FirstClassObject ... irritants )
    {
        super( validateMessage( msg ) );

        _operationName =
                Objects.requireNonNull( operation );
        _code =
                getCode( msg.getValue() );
        _irritants =
                Objects.requireNonNull( irritants );
    }

    private final CachedHolder<Cons> _cachedIrritants =
            new CachedHolder<>(
                    () -> { return Cons.create( getIrritantsImpl() ); } );

    private FirstClassObject[] getIrritantsImpl()
    {
        return _irritants;
    }

    /**
     * @return The list of irritants.
     */
    public Cons getIrritants()
    {
        return _cachedIrritants.get();
    }

    /**
     * @return The unformatted message as passed in the first constructor
     * argument. If the constructor was called with a {@link Code} then
     * the literal enumeration name is returned.
     */
    public String getRawMessage()
    {
        return super.getMessage();
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
     * @return If the name does not match one of the well-defined codes a
     * default code of Code.ERROR is returned, otherwise the respective
     * error code.
     */
    private static Code getCode( String name )
    {
        var result = _nameToCode.get().get( name );

        if ( result != null )
            return result;

        return Code.ERROR;
    }

    /**
     * Sets the name of the throwing operation.  Note that this is only set
     * once and locked.  Further calls to set do <i>not</i> change the
     * operation name set by the first call to this method.
     *
     * @param operation The name of the throwing operation.
     */
    public RuntimeX setOperationName( Symbol operation )
    {
        if ( _operationName == null )
            _operationName = operation;

        return this;
    }

    /**
     * @return The symbolic name of the operation that reported the exception.
     */
    public Symbol getOperationName()
    {
        return _operationName;
    }

    /**
     * @return Never {@code null}.
     */
    public FirstClassObject[] getArguments()
    {
        return _irritants;
    }
    public FirstClassObject getArgument( int idx )
    {
        return getArguments()[ idx ];
    }

    /**
     * @return The exception's code.
     */
    public Code getCode()
    {
        return _code;
    }

    private final CachedHolder<String> _message =
            new CachedHolder<String>( this::makeMessage );

    /**
     * @return This exception's message.
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
                append( getCode() ).
                append( " : " );

        if ( getOperationName() != null )
            result.append( getOperationName() ).append( " : " );

        Object[] fmtArgs = new String[_irritants.length];
        int i = 0;
        for ( var c : _irritants )
            fmtArgs[i++] = FirstClassObject.toString( c );

        final var messageKey = fmtArgs.length > 0 ?
                messageId + "_" + fmtArgs.length :
                messageId;

        if ( ErrorMessages.map.containsKey( messageKey ) )
        {
            return result.toString() + MessageFormat.format(
                    ErrorMessages.map.get( messageKey ),
                    fmtArgs );
        }

        result.append( messageId );

        for ( var c : fmtArgs )
        {
            result.append( " " );
            result.append( c );
        }

        return result.toString();
    }

    /**
     * Scream specific {@code (error ...)} procedure.  Stops the
     * current computation with a runtime error.
     */
    static private Procedure errorProcedure( Environment e )
    {
        return new Procedure( "error", e )
        {
            @Override
            protected Thunk _executeImpl( Environment e, Cons args, Cont<FirstClassObject> c )
                    throws RuntimeX
            {
                checkArgumentCount( 1, Integer.MAX_VALUE, args );

                SchemeString message =
                        Scut.asNotNil( SchemeString.class, args.listRef( 0 ) );

                var splitMessage = message.getValue().split( ":" );

                var irritants =
                        Cons.asArray(
                        Scut.as( Cons.class, args.getCdr() ) );

                if ( splitMessage.length == 1 )
                {
                    RuntimeX result = new RuntimeX(
                            message,
                            irritants );

                    result.setOperationName( e.getName() );

                    throw result;
                }
                else if ( splitMessage.length == 2 )
                {
                    RuntimeX result = new RuntimeX(
                            Symbol.createObject( splitMessage[0].strip() ),
                            SchemeString.make( splitMessage[1].strip() ),
                            irritants );
                    throw result;
                }

                throw mSyntaxErrorF(
                        getName(),
                        message );
            }
        };
    }

    /**
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
    //    Internal error.
    @Deprecated(forRemoval = true  )
    public static RuntimeX mInternalError()
    {
        return new RuntimeX(
                Code.INTERNAL_ERROR );
    }

    //    INTERNAL_ERROR_1 = \
    //    Internal error: {0}.
    public static RuntimeX mInternalError( Object ... msg )
    {
        return new RuntimeX(
                Code.INTERNAL_ERROR,
                SchemeString.make( StringUtil.concatenate( ", ", msg ) ) );
    }

    //    # This functionality is not implemented.
    //    #
    //    NOT_IMPLEMENTED = \
    //    Not implemented.
    public static RuntimeX mNotImplemented()
    {
        return new RuntimeX(
                Code.NOT_IMPLEMENTED );
    }
    public static RuntimeX mNotImplemented( String message )
    {
        return new RuntimeX(
                Code.NOT_IMPLEMENTED,
                SchemeString.make( message ) );
    }

    /**
     *  A symbol is not defined.  The argument should give the symbol name.
     *  Symbol ''{0}'' not defined.
     * @param symbol
     * @return
     */
    public static RuntimeX mSymbolNotDefined( Symbol symbol )
    {
        return new RuntimeX(
                Code.SYMBOL_NOT_DEFINED, symbol );
    }

    //    # Error message is related to the scheme set! special form.  Only bound symbols
    //    # can be assigned using this.  In case the symbol is not bound this error
    //    # message is thrown.
    //    # The argument gives the symbol name that was tried to be assigned.
    //    #
    //    SYMBOL_NOT_ASSIGNABLE_1 = \
    //    Symbol ''{0}'' is not assignable.
    public static RuntimeX mSymbolNotAssignable( Symbol symbol )
    {
        return new RuntimeX(
                Code.SYMBOL_NOT_ASSIGNABLE, symbol );
    }

    //    TOO_MANY_SUBEXPRESSIONS = \
    //    Expression has too many subexpressions.
    public static RuntimeX mTooManySubexpressions()
    {
        return new RuntimeX(
                Code.TOO_MANY_SUBEXPRESSIONS );
    }

    //    TOO_MANY_SUBEXPRESSIONS_1 = \
    //    Expression has too many subexpressions: {0}.
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
    //    Syntax error.
    public static RuntimeX mSyntaxError()
    {
        return new RuntimeX(
                Code.SYNTAX_ERROR );
    }

    //    SYNTAX_ERROR_1 = \
    //    Syntax error in {0}
    public static RuntimeX mSyntaxError( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.SYNTAX_ERROR,
                fco );
    }
    public static RuntimeX mSyntaxErrorF( Symbol function, FirstClassObject fco )
    {
        return new RuntimeX(
                function,
                SchemeString.make( Code.SYNTAX_ERROR.toString() ),
                fco );
    }

    //    DEFINE_ERROR = \
    //    Invalid identifier for define.
    public static RuntimeX mDefineError()
    {
        return new RuntimeX(
                Code.DEFINE_ERROR );
    }

    //    EXPECTED_PROPER_LIST = \
    //    Expected proper list.
    public static RuntimeX mExpectedProperList()
    {
        return new RuntimeX(
                Code.EXPECTED_PROPER_LIST );
    }

    //    EXPECTED_PROPER_LIST_1 = \
    //    Expected proper list.  Received {0}
    public static RuntimeX mExpectedProperList(
            FirstClassObject actual )
    {
        return new RuntimeX(
                Code.EXPECTED_PROPER_LIST,
                actual );
    }

    //    Index out of bounds: {0}
    public static RuntimeX mIndexOutOfBounds( long actual )
    {
        return new RuntimeX(
                Code.INDEX_OUT_OF_BOUNDS,
                Int.make( actual ) );
    }

    //    Attempt to call non-procedural object {0}.
    public static RuntimeX mCalledNonProcedural( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.CALLED_NON_PROCEDURAL,
                fco );
    }

    //    INVALID_ASSOC_LIST_1 = \
    //    Invalid association list: {0}
    public static RuntimeX mInvalidAssocList( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.INVALID_ASSOC_LIST,
                fco );
    }

    //    CAR_FAILED_1 = \
    //    Can't get car for {0}.
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
                SchemeString.make( FirstClassObject.typename( expected ) ),
                SchemeString.make( msg ) );
    }

    //    TYPE_ERROR_3 = \
    //    Argument {2} has wrong type.  Expected {0} but found {1}.
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError( Class<T1> expected, Class<T2> found, int argumentIdx )
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                SchemeString.make( FirstClassObject.typename( expected ) ),
                SchemeString.make( FirstClassObject.typename( found ) ),
                Int.createObject( argumentIdx ) );
    }

    //    NOT_ENOUGH_ARGUMENTS_1 = \
    //    Wrong number of arguments.  Expected at least {0}.
    public static RuntimeX mNotEnoughArguments( long minExpected )
    {
        return new RuntimeX(
                Code.NOT_ENOUGH_ARGUMENTS,
                Int.make( minExpected ) );
    }

    //    NOT_ENOUGH_ARGUMENTS_2 = \
    //    Wrong number of arguments.  Expected at least {0} but received {1}.
    public static RuntimeX mNotEnoughArguments( long minExpected, long received )
    {
        return new RuntimeX(
                Code.NOT_ENOUGH_ARGUMENTS,
                Int.make( minExpected ),
                Int.make( received ) );
    }

    //    TOO_MANY_ARGUMENTS_1 = \
    //    Wrong number of arguments.  Expected at most {0}.
    public static RuntimeX mTooManyArguments( long maxExpected )
    {
        return new RuntimeX(
                Code.TOO_MANY_ARGUMENTS,
                Int.make( maxExpected ) );
    }

    //    TOO_MANY_ARGUMENTS_2 = \
    //    Wrong number of arguments.  Expected at most {0} but received {1}.
    public static RuntimeX mTooManyArguments( long maxExpected, long received )
    {
        return new RuntimeX(
                Code.TOO_MANY_ARGUMENTS,
                Int.make( maxExpected ),
                Int.make( received ) );
    }

    //    # arg 0: Number of received parameters
    public static RuntimeX mWrongNumberOfArguments( long received )
    {
        return new RuntimeX(
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                Int.make( received ) );
    }

    //    # arg 0: Number of expected parameters
    //    # arg 1: Number of received parameters
    //    #
    //    WRONG_NUMBER_OF_ARGUMENTS_2 = \
    //    Wrong number of arguments.  Expected {0} but received {1}.
    public static RuntimeX mWrongNumberOfArguments( long expected, long received )
    {
        return new RuntimeX(
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                Int.make( expected ),
                Int.make( received ) );
    }

    //    # Procedure (map) specific.  First passed argument has to be a procedure, all
    //    # remaining arguments have to be lists of the same length.
    //    #
    //    REQUIRES_EQUIVALENT_CONS_LEN = \
    //    All passed lists have to have the same length.
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
    //    Bad binding in {0} syntax: {1}
    public static RuntimeX mBadBinding( FirstClassObject syntax, FirstClassObject binding )
    {
        return new RuntimeX(
                Code.BAD_BINDING,
                syntax,
                binding );
    }

    //    BAD_CLAUSE_1 = \
    //    Bad clause: {0}
    public static RuntimeX mBadClause( FirstClassObject clause )
    {
        return new RuntimeX(
                Code.BAD_CLAUSE,
                clause );
    }

    //    DIVISION_BY_ZERO = \
    //    Division by zero.
    public static RuntimeX mDivisionByZero()
    {
        return new RuntimeX(
                Code.DIVISION_BY_ZERO );
    }

    //    # It has been tried to write to or read from a closed port.
    //    #
    //    PORT_CLOSED = \
    //    Port is closed.
    public static RuntimeX mPortClosed()
    {
        return new RuntimeX(
                Code.PORT_CLOSED );
    }

    //    # Used in the port related input procedures.
    //    #
    //    EXPECTED_INPUT_PORT = \
    //    Tried to read from output port.
    public static RuntimeX mExpectedInputPort()
    {
        return new RuntimeX(
                Code.EXPECTED_INPUT_PORT );
    }

    //    # Used in the port related output procedures.
    //    #
    //    EXPECTED_OUTPUT_PORT = \
    //    Tried to write on input port.
    public static RuntimeX mExpectedOutputPort()
    {
        return new RuntimeX(
                Code.EXPECTED_OUTPUT_PORT );
    }

    //    # An I/O error has occurred.
    //    #
    //    # arg 0:  The system message.
    //    #
    //    IO_ERROR_1 = \
    //    Input/Output operation failed: {0}
    public static RuntimeX mIoError( IOException e )
    {
        return new RuntimeX(
                Code.IO_ERROR,
                SchemeString.make( e.getMessage() ) );
    }

    //    DUPLICATE_FORMAL_1 = \
    //    Duplicate formal argument name: {0}
    public static RuntimeX mDuplicateFormal( FirstClassObject formal )
    {
        return new RuntimeX(
                Code.DUPLICATE_FORMAL,
                formal );
    }

    //    INVALID_FORMALS_1 = \
    //    Invalid formal argument list: {0}
    public static RuntimeX mInvalidFormals( FirstClassObject formals )
    {
        return new RuntimeX(
                Code.INVALID_FORMALS,
                formals );
    }

    //    CLASS_NOT_FOUND_1 = \
    //    Class {0} not found.
    public static RuntimeX mClassNotFound( String name )
    {
        return new RuntimeX(
                Code.CLASS_NOT_FOUND,
                SchemeString.make( name ) );
    }

    //    FIELD_NOT_FOUND_1 = \
    //    Field not found: {0}
    public static RuntimeX mFieldNotFound( String name )
    {
        return new RuntimeX(
                Code.FIELD_NOT_FOUND,
                SchemeString.make( name ) );
    }

    //    METHOD_NOT_FOUND_1 = \
    //    Method not found: {0}
    public static RuntimeX mMethodNotFound( String name )
    {
        return new RuntimeX(
                Code.METHOD_NOT_FOUND,
                SchemeString.make( name ) );
    }
    //    METHOD_NOT_FOUND_1 = \
    //    Method not found: {0}
    public static RuntimeX mMethodNotFound( String name, Cons arguments )
    {
        return new RuntimeX(
                Code.METHOD_NOT_FOUND,
                SchemeString.make( name + FirstClassObject.toString( arguments ) ) );
    }

    //    ILLEGAL_ACCESS_1 = \
    //    No access to {0}.
    public static RuntimeX mIllegalAccess( String name )
    {
        return new RuntimeX(
                Code.ILLEGAL_ACCESS,
                SchemeString.make( name ) );
    }

    //    # A method invoked by reflection threw an exception.  Note that this exception
    //    # always has to be different from a RuntimeX, this case has to be handled.
    //    #
    //    INVOCATION_EXCEPTION_2 = \
    //    Invoked method {0} threw exception {1}.  See logfile for full exception.
    public static RuntimeX mInvocationException( Executable method, Throwable e )
    {
        return new RuntimeX(
                Code.INVOCATION_EXCEPTION,
                SchemeString.make( method.toString() ),
                SchemeString.make( e.toString() ) );
    }

    //    # It has been tried to access instance information on a class.
    //    #
    //    CANT_ACCESS_INSTANCE = \
    //    Tried to access instance information on class object
    public static RuntimeX mCannotAccessInstance()
    {
        return new RuntimeX(
                Code.CANNOT_ACCESS_INSTANCE );
    }

    //    # arg 0: The class name that was tried to instantiate.
    //    #
    //    CREATION_FAILED = \
    //    Can't create an instance of {0}.
    public static RuntimeX mCreationFailed( String name )
    {
        return new RuntimeX(
                Code.CREATION_FAILED,
                SchemeString.make( name ) );
    }

    //    # An illegal argument was passed into a reflective invocation of a method.
    //    # Used in SchemeObject.
    //    #
    //    ILLEGAL_ARGUMENT_1 = \
    //    Illegal argument for {0}.
    public static RuntimeX mIllegalArgument( String name )
    {
        return new RuntimeX(
                Code.ILLEGAL_ARGUMENT,
                SchemeString.make( name ) );
    }

    //    # An unbalanced quote has been found.
    //    #
    //    SCAN_UNBALANCED_QUOTE = \
    //    Unbalanced quote.
    public static RuntimeX mScanUnbalancedQuote()
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_QUOTE );
    }

    //    SCAN_UNBALANCED_QUOTE_2 = \
    //    Unbalanced quote found at line {0}, column {1}.
    public static RuntimeX mScanUnbalancedQuote( SourcePosition position )
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_QUOTE,
                Int.make( position.line() ),
                Int.make( position.column() ) );
    }

    //    SCAN_UNEXPECTED_CHAR_3 = \
    //    Unexpected character ''{2}'' found at line {0}, column {1}.
    public static RuntimeX mScanUnexpectedChar( SourcePosition position, String character )
    {
        return new RuntimeX(
                Code.SCAN_UNEXPECTED_CHAR,
                Int.make( position.line() ),
                Int.make( position.column() ),
                SchemeString.make( character ) );
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
    //    Expected {0}.
    public static RuntimeX mParseExpected( Tk token )
    {
        return new RuntimeX(
                Code.PARSE_EXPECTED,
                SchemeString.make( token.toString() ) );
    }

    //    # The parser found a premature end of file.
    //    #
    //    PARSE_UNEXPECTED_EOF = \
    //    Unexpected end of file.
    public static RuntimeX mParseUnexpectedEof()
    {
        return new RuntimeX(
                Code.PARSE_UNEXPECTED_EOF );
    }

    //    # The parser found a token that was not expected.
    //    #
    //    PARSE_UNEXPECTED_1 = \
    //    Unexpected {0}.
    public static RuntimeX mParseUnexpected( Token token )
    {
        return new RuntimeX(
                Code.PARSE_UNEXPECTED,
                SchemeString.make( token.toString() ) );
    }

    //    INTERRUPTED = \
    //    Computation interrupted.
    public static RuntimeX mInterrupted()
    {
        return new RuntimeX(
                Code.INTERRUPTED );
    }

    //    CANT_MODIFY_CONSTANT = \
    //    Tried to modify constant.
    public static RuntimeX mCannotModifyConstant()
    {
        return new RuntimeX(
                Code.CANNOT_MODIFY_CONSTANT );
    }

    //    CANT_MODIFY_CONSTANT_1 = \
    //    Tried to modify constant: {0}
    public static RuntimeX mCannotModifyConstant( FirstClassObject constant )
    {
        return new RuntimeX(
                Code.CANNOT_MODIFY_CONSTANT,
                constant );
    }

    //    # arg 0: The name of the class that has been tried to instantiate as a proxy.
    //    #
    //    NO_PROXY_1 = \
    //    Not a proxy interface: {0}
    public static RuntimeX mNoProxy( String name )
    {
        return new RuntimeX(
                Code.NO_PROXY,
                SchemeString.make( name ) );
    }

    //    PROXY_CANT_INSTANCIATE = \
    //    Can't instantiate proxy.
    public static RuntimeX mProxyCannotInstantiate()
    {
        return new RuntimeX(
                Code.PROXY_CANNOT_INSTANTIATE );
    }
    //    PROXY_CANT_INSTANCIATE_1 = \
    //    Can't instantiate proxy: {0}
    public static RuntimeX mProxyCannotInstantiate( String name )
    {
        return new RuntimeX(
                Code.PROXY_CANNOT_INSTANTIATE,
                SchemeString.make( name ) );
    }

    //    # Will be used in automatic regression testing.
    //    #
    //    TEST_FAILED_2 = \
    //    Test {0}#{1} failed.
//    public static RuntimeX mTestFailed( String group, String name )
//    {
//        return new RuntimeX(
//                Code.TEST_FAILED,
//                group,
//                name );
//    }

    //    ONLY_IN_QUASIQUOTE_CONTEXT = \
    //    Only applicable in quasiquote template.
    public static RuntimeX mOnlyInQuasiquoteContext()
    {
        return new RuntimeX(
                Code.ONLY_IN_QUASIQUOTE_CONTEXT );
    }

    //    RADIX_NOT_SUPPORTED_2 = \
    //    Radix {0} is not supported.  Maximum radix is {1}.
    public static RuntimeX mRadixNotSupported( int radix, int maxRadix )
    {
        return new RuntimeX(
                Code.RADIX_NOT_SUPPORTED,
                Int.createObject( radix ),
                Int.createObject( maxRadix ) );
    }

    //    # A list contained a duplicate element.  Used in case-syntax.
    //    #
    //    DUPLICATE_ELEMENT_1 = \
    //    Duplicate element : {0}
    public static RuntimeX mDuplicateElement( FirstClassObject duplicate )
    {
        return new RuntimeX(
                Code.DUPLICATE_ELEMENT,
                duplicate );
    }

    // EXPECTED_BINARY_PORT_1 = \
    // Expected binary port, got {0}.
    public static RuntimeX mExpectedBinaryPort( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.EXPECTED_BINARY_PORT,
                fco );
    }

    // EXPECTED_TEXTUAL_PORT_1 = \
    // Expected textual port, got {0}.
    public static RuntimeX mExpectedTextualPort( FirstClassObject fco )
    {
        return new RuntimeX(
                Code.EXPECTED_TEXTUAL_PORT,
                fco );
    }

    // SCAN_UNBALANCED_COMMENT_2 = \
    // Unbalanced comment found at line {0}, column {1}.
    public static RuntimeX mScanUnbalancedComment( SourcePosition position )
    {
        return new RuntimeX(
                Code.SCAN_UNBALANCED_COMMENT,
                Int.make( position.line() ),
                Int.make( position.column() ) );
    }

    // RANGE_EXCEEDED_1 = \
    // Range exceeded. Actual {0}, expected {1}.
    public static RuntimeX mRangeExceeded( FirstClassObject fco, String rangeDescription )
    {
        return new RuntimeX(
                Code.RANGE_EXCEEDED,
                fco,
                SchemeString.make( rangeDescription ) );
    }

    /**
     * RAISE is a special exception used in the implementation of the
     * r7rs 6.11 {@code (raise obj)} and {@code (raise-continuable)}
     * implementations.
     * <p>
     * The continuable parameter may be {@code null}, resulting in a
     * non-continuable exception.  If a continuation is passed then the
     * exception is continuable and the result of the exception handler
     * defined by {@code (with-exception-handler ...)} will be passed.
     *
     * @param continuable The continuation hat handles the raise result if
     * continuable or {@code null} if the exception is non-continuable.
     * @param fco The raise procedure's parameter.
     * @return The initialized exception.
     */
    // RAISE_1 = \
    // ...
    public static RuntimeX mRaise( Cont<FirstClassObject> continuable, FirstClassObject fco )
    {
        return new RuntimeX(
                Code.RAISE,
                continuable == null ?
                        null :
                        new Continuation( continuable, null ),
                fco );
    }

    /**
     * The exception that is finally thrown when the exception
     * handler of a (raise ...)-call finished.
     * @return The initialized exception.
     */
    // ...
    public static RuntimeX mNotContinuable()
    {
        return new RuntimeX(
                Code.NOT_CONTINUABLE );
    }
}
