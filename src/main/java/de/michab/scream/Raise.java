/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import java.io.IOException;
import java.lang.reflect.Executable;

import org.smack.util.StringUtil;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.Continuation;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.SchemeString;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.Continuation.Cont;
import de.michab.scream.util.SourcePosition;

/**
 * A collection of error creation operations that are to be used in the
 * Java-based Scheme-operation implementations.
 *
 * @author micbinz
 */
public class Raise
{
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

    /**
     * An error related to the scheme set! special form.  Only bound symbols
     * can be assigned.
     *
     * @param symbol The name that could not be assigned.
     * @return An initialized exception.
     */
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
    public static RuntimeX mSyntaxErrorF( Symbol function, String fco )
    {
        return new RuntimeX(
                function,
                SchemeString.make( Code.SYNTAX_ERROR.toString() ),
                SchemeString.make( fco ) );
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
    public static RuntimeX mWrongNumberOfArgumentsF(
            Symbol function,
            long expected,
            long received )
    {
        return new RuntimeX(
                function,
                SchemeString.make( Code.WRONG_NUMBER_OF_ARGUMENTS.toString() ),
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
    public static RuntimeX mCannotModifyConstantF( Symbol function, FirstClassObject constant )
    {
        return new RuntimeX(
                function,
                SchemeString.make( Code.CANNOT_MODIFY_CONSTANT.toString() ),
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

    /**
     * A list contained a duplicate element.  Used in case-syntax.
     *
     * @param duplicate The duplicate element.
     * @return
     */
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
    public static RuntimeX mNotContinuable()
    {
        return new RuntimeX(
                Code.NOT_CONTINUABLE );
    }

    /**
     * Hide constructor.
     */
    private Raise()
    {
        throw new AssertionError();
    }
}
