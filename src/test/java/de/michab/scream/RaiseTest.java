/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.io.FileNotFoundException;
import java.io.IOException;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.Real;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.SourcePosition;

public class RaiseTest extends ScreamBaseTest
{
    @Test
    public void internalError() throws Exception
    {
        var se = new RuntimeX( str( Code.INTERNAL_ERROR.name() ) );

        assertEquals(
                Code.INTERNAL_ERROR,
                se.getCode() );
    }

    private RuntimeX validateMessageAndType( RuntimeX x, Code c, Object ... args )
    {
        var msg = x.getMessage();
        assertNotNull( msg );
        assertEquals(
                c,
                x.getCode() );
        if ( args.length != 0  )
            assertArrayEquals( args, x.getArguments() );
        return x;
    }

    @Test
    public void internalError1() throws Exception
    {
        validateMessageAndType(
                Raise.mInternalError( "313" ),
                Code.INTERNAL_ERROR,
                str("313") );
    }
    @Test
    public void notImplemented() throws Exception
    {
        validateMessageAndType(
                Raise.mNotImplemented(),
                Code.NOT_IMPLEMENTED );
    }
    @Test
    public void notImplemented1() throws Exception
    {
        validateMessageAndType(
                Raise.mNotImplemented( "donald" ),
                Code.NOT_IMPLEMENTED,
                str( "donald" ) );
    }
    @Test
    public void symbolNotDefined() throws Exception
    {
        validateMessageAndType(
                Raise.mSymbolNotDefined( s313 ),
                Code.SYMBOL_NOT_DEFINED,
                s313 );
    }
    @Test
    public void symbolNotAssignable() throws Exception
    {
        validateMessageAndType(
                Raise.mSymbolNotAssignable( s313 ),
                Code.SYMBOL_NOT_ASSIGNABLE,
                s313 );
    }
    @Test
    public void tooManySubexpressions() throws Exception
    {
        validateMessageAndType(
                Raise.mTooManySubexpressions(),
                Code.TOO_MANY_SUBEXPRESSIONS );
    }
    @Test
    public void tooManySubexpressions1() throws Exception
    {
        validateMessageAndType(
                Raise.mTooManySubexpressions( s313 ),
                Code.TOO_MANY_SUBEXPRESSIONS,
                s313 );
    }
    @Test
    public void syntaxError() throws Exception
    {
        validateMessageAndType(
                Raise.mSyntaxError(),
                Code.SYNTAX_ERROR );
    }
    @Test
    public void syntaxError1() throws Exception
    {
        validateMessageAndType(
                Raise.mSyntaxError( s313 ),
                Code.SYNTAX_ERROR,
                s313 );
    }
    @Test
    public void defineError() throws Exception
    {
        validateMessageAndType(
                Raise.mDefineError(),
                Code.DEFINE_ERROR );
    }
    @Test
    public void expectedProperList() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedProperList(),
                Code.EXPECTED_PROPER_LIST );
    }
    @Test
    public void expectedProperList1() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedProperList( s313 ),
                Code.EXPECTED_PROPER_LIST,
                s313 );
    }
    @Test
    public void indexOutOfBounds() throws Exception
    {
        validateMessageAndType(
                Raise.mIndexOutOfBounds( 7 ),
                Code.INDEX_OUT_OF_BOUNDS,
                i(7) );
    }
    @Test
    public void calledNonProcedural() throws Exception
    {
        validateMessageAndType(
                Raise.mCalledNonProcedural( s313 ),
                Code.CALLED_NON_PROCEDURAL,
                s313 );
    }
    @Test
    public void invalidAssocList() throws Exception
    {
        validateMessageAndType(
                Raise.mInvalidAssocList( s313 ),
                Code.INVALID_ASSOC_LIST,
                s313 );
    }
    @Test
    public void carFailed() throws Exception
    {
        validateMessageAndType(
                Raise.mCarFailed( s313 ),
                Code.CAR_FAILED,
                s313 );
    }
    @Test
    public void typeError2() throws Exception
    {
        validateMessageAndType(
                Raise.mTypeError( Cons.class, d(313) ),
                Code.TYPE_ERROR,
                str( FirstClassObject.typename( Cons.class ) ),
                str( FirstClassObject.typename( Real.class ) + "=313.0" ) );
    }
    @Test
    public void typeError3() throws Exception
    {
        validateMessageAndType(
                Raise.mTypeError( Cons.class, Port.class, 313 ),
                Code.TYPE_ERROR,
                str( FirstClassObject.typename( Cons.class ) ),
                str( FirstClassObject.typename( Port.class ) ),
                i313 );
    }
    @Test
    public void notEnoughArguments() throws Exception
    {
        validateMessageAndType(
                Raise.mNotEnoughArguments( 313 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                i313 );
    }
    @Test
    public void notEnoughArguments2() throws Exception
    {
        validateMessageAndType(
                Raise.mNotEnoughArguments( 313, 314 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void tooManyArguments1() throws Exception
    {
        validateMessageAndType(
                Raise.mTooManyArguments( 313 ),
                Code.TOO_MANY_ARGUMENTS,
                i313 );
    }
    @Test
    public void tooManyArguments2() throws Exception
    {
        validateMessageAndType(
                Raise.mTooManyArguments( 313, 314 ),
                Code.TOO_MANY_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void wrongNumberOfArguments1() throws Exception
    {
        validateMessageAndType(
                Raise.mWrongNumberOfArguments( 313 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                i313 );
    }
    @Test
    public void wrongNumberOfArguments2() throws Exception
    {
        validateMessageAndType(
                Raise.mWrongNumberOfArguments( 313, 314 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void requiresEquivalentConsLen() throws Exception
    {
        validateMessageAndType(
                Raise.mRequiresEqivalentConsLength(),
                Code.REQUIRES_EQUIVALENT_CONS_LEN );
    }
    @Test
    public void badBinding_2() throws Exception
    {
        validateMessageAndType(
                Raise.mBadBinding( s313, s1 ),
                Code.BAD_BINDING,
                s313,
                s1 );
    }
    @Test
    public void badClause_3() throws Exception
    {
        validateMessageAndType(
                Raise.mBadClause( s2 ),
                Code.BAD_CLAUSE,
                s2 );
    }
    @Test
    public void divisionByZero() throws Exception
    {
        validateMessageAndType(
                Raise.mDivisionByZero(),
                Code.DIVISION_BY_ZERO );
    }
    @Test
    public void portClosed() throws Exception
    {
        validateMessageAndType(
                Raise.mPortClosed(),
                Code.PORT_CLOSED );
    }
    @Test
    public void expectedInputPort() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedInputPort(),
                Code.EXPECTED_INPUT_PORT );
    }
    @Test
    public void expectedOutputPort() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedOutputPort(),
                Code.EXPECTED_OUTPUT_PORT );
    }
    @Test
    public void ioError1() throws Exception
    {
        IOException iox = new FileNotFoundException( "not found" );

        validateMessageAndType(
                Raise.mIoError( iox ),
                Code.IO_ERROR,
                str( iox.getMessage() ) );
    }
    @Test
    public void duplicateFormal1() throws Exception
    {
        validateMessageAndType(
                Raise.mDuplicateFormal( s4 ),
                Code.DUPLICATE_FORMAL,
                s4 );
    }
    @Test
    public void invalidFormals1() throws Exception
    {
        validateMessageAndType(
                Raise.mInvalidFormals( s4 ),
                Code.INVALID_FORMALS,
                s4 );
    }
    @Test
    public void classNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mClassNotFound( what ),
                Code.CLASS_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void fieldNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mFieldNotFound( what ),
                Code.FIELD_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void methodNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mMethodNotFound( what ),
                Code.METHOD_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void methodNotFound2() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mMethodNotFound( what, Cons.create( i1, i2 ) ),
                Code.METHOD_NOT_FOUND,
                str( "what(1 2)" ) );
    }
    @Test
    public void illegalAccess1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mIllegalAccess( what ),
                Code.ILLEGAL_ACCESS,
                str( what ) );
    }
    @Test
    public void invocationException1() throws Exception
    {
        var executable = getClass().getMethods()[0];
        var throwable = new Exception( "testException" );

        validateMessageAndType(
                Raise.mInvocationException( executable, throwable ),
                Code.INVOCATION_EXCEPTION,
                str( executable.toString() ),
                str( throwable.toString() ) );
    }
    @Test
    public void cannotAcessInstance1() throws Exception
    {
        validateMessageAndType(
                Raise.mCannotAccessInstance(),
                Code.CANNOT_ACCESS_INSTANCE );
    }
    @Test
    public void creationFailed1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mCreationFailed( what ),
                Code.CREATION_FAILED,
                str( what ) );
    }
    @Test
    public void illegalArgument() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mIllegalArgument( what ),
                Code.ILLEGAL_ARGUMENT,
                str( what ) );
    }
    @Test
    public void scanUnbalancedQuote() throws Exception
    {
        validateMessageAndType(
                Raise.mScanUnbalancedQuote(),
                Code.SCAN_UNBALANCED_QUOTE );
    }
    @Test
    public void scanUnbalancedQuote2() throws Exception
    {
        validateMessageAndType(
                Raise.mScanUnbalancedQuote(
                        new SourcePosition( 1, 2, null ) ),
                Code.SCAN_UNBALANCED_QUOTE,
                i1,
                i2 );
    }
    @Test
    public void scanUnexpectedCharacter() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mScanUnexpectedChar(
                        new SourcePosition( 1, 2, null ),
                        what ),
                Code.SCAN_UNEXPECTED_CHAR,
                i1,
                i2,
                str( what ) );
    }
    @Test
    public void error() throws Exception
    {
        validateMessageAndType(
                Raise.mError(),
                Code.ERROR );
    }
    @Test
    public void parseExpected1() throws Exception
    {
        validateMessageAndType(
                Raise.mParseExpected( Tk.Dot ),
                Code.PARSE_EXPECTED,
                str( Tk.Dot.toString() ) );
    }
    @Test
    public void parseUnexpectedEof() throws Exception
    {
        validateMessageAndType(
                Raise.mParseUnexpectedEof(),
                Code.PARSE_UNEXPECTED_EOF );
    }
    @Test
    public void parseUnexpected1() throws Exception
    {
        var token = new Token(
                Token.Tk.Dot,
                new SourcePosition( -1, -1, "parseUnexpected1" ) );

        validateMessageAndType(
                Raise.mParseUnexpected( token ),
                Code.PARSE_UNEXPECTED,
                str( token.toString() ) );
    }
    @Test
    public void interrupted() throws Exception
    {
        validateMessageAndType(
                Raise.mInterrupted(),
                Code.INTERRUPTED );
    }
    @Test
    public void cannotModifyConstant() throws Exception
    {
        validateMessageAndType(
                Raise.mCannotModifyConstant(),
                Code.CANNOT_MODIFY_CONSTANT );
    }
    @Test
    public void cannotModifyConstant1() throws Exception
    {
        validateMessageAndType(
                Raise.mCannotModifyConstant( s1 ),
                Code.CANNOT_MODIFY_CONSTANT,
                s1 );
    }
    @Test
    public void noProxy1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mNoProxy( what ),
                Code.NO_PROXY,
                str( what ) );
    }
    @Test
    public void ProxyCannotInstantiate() throws Exception
    {
        validateMessageAndType(
                Raise.mProxyCannotInstantiate(),
                Code.PROXY_CANNOT_INSTANTIATE );
    }
    @Test
    public void ProxyCannotInstantiate1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                Raise.mProxyCannotInstantiate( what ),
                Code.PROXY_CANNOT_INSTANTIATE,
                str( what ) );
    }
    @Test
    public void onlyInQuasiquoteContext() throws Exception
    {
        validateMessageAndType(
                Raise.mOnlyInQuasiquoteContext(),
                Code.ONLY_IN_QUASIQUOTE_CONTEXT );
    }
    @Test
    public void radixNotSupported2() throws Exception
    {
        validateMessageAndType(
                Raise.mRadixNotSupported( 1, 2 ),
                Code.RADIX_NOT_SUPPORTED,
                i(1),
                i(2) );
    }
    @Test
    public void duplicateElement_1() throws Exception
    {
        validateMessageAndType(
                Raise.mDuplicateElement( s3 ),
                Code.DUPLICATE_ELEMENT,
                s3 );
    }
    @Test
    public void expectedBinaryPort_1() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedBinaryPort( s3 ),
                Code.EXPECTED_BINARY_PORT,
                s3 );
    }
    @Test
    public void expectedTextualPort_1() throws Exception
    {
        validateMessageAndType(
                Raise.mExpectedTextualPort( s3 ),
                Code.EXPECTED_TEXTUAL_PORT,
                s3 );
    }
    @Test
    public void scanUnbalancedComment() throws Exception
    {
        validateMessageAndType(
                Raise.mScanUnbalancedComment( new SourcePosition( 1, 1, null ) ),
                Code.SCAN_UNBALANCED_COMMENT,
                i1,
                i1);
    }
    @Test
    public void rangeExceeded() throws Exception
    {
        validateMessageAndType(
                Raise.mRangeExceeded( i313, "> 256" ),
                Code.RANGE_EXCEEDED,
                i313,
                str( "> 256" ) );
    }
}
