/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

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
import de.michab.scream.util.ErrorMessages;
import de.michab.scream.util.SourcePosition;

public class RuntimeXTest extends ScreamBaseTest
{
    @Test
    public void blankError() throws Exception
    {
        try
        {
            new RuntimeX( str("") );
            fail();
        }
        catch ( IllegalArgumentException expected )
        {
        }
    }

    @Test
    public void paramMismatch() throws Exception
    {
        // BAD_BINDING is only defined by BAD_BINDING_2, this
        // tests the fallback to BAD_BINDING.
        assertFalse(
                ErrorMessages.map.containsKey( Code.BAD_BINDING.toString() )
        );

        var se = new RuntimeX( Code.BAD_BINDING, s( "unknown" ) );

        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
    }

    @Test
    public void internalError() throws Exception
    {
        var se = new RuntimeX( str( Code.INTERNAL_ERROR.name() ) );

        assertEquals(
                Code.INTERNAL_ERROR,
                se.getCode() );
    }

    @Test
    public void undefinedName() throws Exception
    {
        final var name = "Gobbledigoog";

        var se = new RuntimeX( str( name ) );

        assertEquals(
                Code.ERROR,
                se.getCode() );
        assertNotNull(
                se.getArguments() );
        assertEquals(
                name,
                se.getRawMessage() );
    }

    @Test
    public void undefinedNameAndIrritants() throws Exception
    {
        var se = new RuntimeX(
                str( "Gobbledigoog" ),
                i313,
                i1,
                s313,
                str( "A string." ) );

        assertEquals(
                Code.ERROR,
                se.getCode() );

        var irritants = se.getArguments();

        assertEquals(
                4, irritants.length );
        assertEqualq(
                i313,
                (FirstClassObject)irritants[0] );
        assertEqualq(
                i1,
                (FirstClassObject)irritants[1] );
        assertEqualq(
                s313,
                (FirstClassObject)irritants[2] );
        assertEqualq(
                str("A string."),
                (FirstClassObject)irritants[3] );
    }

    @Test
    public void _badBinding() throws Exception
    {
        var se = new RuntimeX(
                str( Code.BAD_BINDING.name() ),
                str( "a1" ),
                str( "a2" ) );

        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
        assertEquals(
                Code.BAD_BINDING.toString(),
                se.getRawMessage() );

        assertTrue( se.getMessage().contains( "a1" ) );
        assertTrue( se.getMessage().contains( "a2" ) );
    }

    @Test
    public void operationName() throws Exception
    {
        var se = RuntimeX.mDivisionByZero().setOperationName( s313 );

        assertEquals(
                Code.DIVISION_BY_ZERO,
                se.getCode() );

        var splitMessage = se.getMessage().split( " : " );
        assertEquals( s313.toString(), splitMessage[1] );
    }

    @Test
    public void unknownCode() throws Exception
    {
        var rx = new RuntimeX( str( "duck" ) );
        assertEquals( Code.ERROR, rx.getCode() );
        assertEquals( Code.ERROR + " : duck", rx.getMessage() );
    }

    @Test
    public void unknownName() throws Exception
    {
            var sex = new RuntimeX(
                    str( ".UNKNOWN" ),
                    i1, i2, i3 );
            assertEquals( Code.ERROR, sex.getCode() );
            assertEquals( Code.ERROR + " : .UNKNOWN 1 2 3", sex.getMessage() );
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
    public void _m1_internalError1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInternalError( "313" ),
                Code.INTERNAL_ERROR,
                str("313") );
    }
    @Test
    public void _0_notImplemented() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotImplemented(),
                Code.NOT_IMPLEMENTED );
    }
    @Test
    public void _0_notImplemented1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotImplemented( "donald" ),
                Code.NOT_IMPLEMENTED,
                str( "donald" ) );
    }
    @Test
    public void _1_symbolNotDefined() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSymbolNotDefined( s313 ),
                Code.SYMBOL_NOT_DEFINED,
                s313 );
    }
    @Test
    public void _2_symbolNotAssignable() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSymbolNotAssignable( s313 ),
                Code.SYMBOL_NOT_ASSIGNABLE,
                s313 );
    }
    @Test
    public void _3_tooManySubexpressions() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManySubexpressions(),
                Code.TOO_MANY_SUBEXPRESSIONS );
    }
    @Test
    public void _3_tooManySubexpressions1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManySubexpressions( s313 ),
                Code.TOO_MANY_SUBEXPRESSIONS,
                s313 );
    }
    @Test
    public void _4_syntaxError() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSyntaxError(),
                Code.SYNTAX_ERROR );
    }
    @Test
    public void _4_syntaxError1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSyntaxError( s313 ),
                Code.SYNTAX_ERROR,
                s313 );
    }
    @Test
    public void _5_defineError() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDefineError(),
                Code.DEFINE_ERROR );
    }
    @Test
    public void _6_expectedProperList() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedProperList(),
                Code.EXPECTED_PROPER_LIST );
    }
    @Test
    public void _6_expectedProperList1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedProperList( s313 ),
                Code.EXPECTED_PROPER_LIST,
                s313 );
    }
    @Test
    public void _7_indexOutOfBounds() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mIndexOutOfBounds( 7 ),
                Code.INDEX_OUT_OF_BOUNDS,
                i(7) );
    }
    @Test
    public void _8_calledNonProcedural() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCalledNonProcedural( s313 ),
                Code.CALLED_NON_PROCEDURAL,
                s313 );
    }
    @Test
    public void _9_invalidAssocList() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInvalidAssocList( s313 ),
                Code.INVALID_ASSOC_LIST,
                s313 );
    }
    @Test
    public void _10_carFailed() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCarFailed( s313 ),
                Code.CAR_FAILED,
                s313 );
    }
    @Test
    public void _11_typeError2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, d(313) ),
                Code.TYPE_ERROR,
                str( FirstClassObject.typename( Cons.class ) ),
                str( FirstClassObject.typename( Real.class ) + "=313.0" ) );
    }
    @Test
    public void _11_typeError3() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, Port.class, 313 ),
                Code.TYPE_ERROR,
                str( FirstClassObject.typename( Cons.class ) ),
                str( FirstClassObject.typename( Port.class ) ),
                i313 );
    }
    @Test
    public void _12_notEnoughArguments() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotEnoughArguments( 313 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                i313 );
    }
    @Test
    public void _12_notEnoughArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotEnoughArguments( 313, 314 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void _13_tooManyArguments1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManyArguments( 313 ),
                Code.TOO_MANY_ARGUMENTS,
                i313 );
    }
    @Test
    public void _13_tooManyArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManyArguments( 313, 314 ),
                Code.TOO_MANY_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void _14_wrongNumberOfArguments1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mWrongNumberOfArguments( 313 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                i313 );
    }
    @Test
    public void _14_wrongNumberOfArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mWrongNumberOfArguments( 313, 314 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                i313,
                i(314) );
    }
    @Test
    public void _15_requiresEquivalentConsLen() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRequiresEqivalentConsLength(),
                Code.REQUIRES_EQUIVALENT_CONS_LEN );
    }
    @Test
    public void _16_badBinding_2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mBadBinding( s313, s1 ),
                Code.BAD_BINDING,
                s313,
                s1 );
    }
    @Test
    public void _17_badClause_3() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mBadClause( s2 ),
                Code.BAD_CLAUSE,
                s2 );
    }
    @Test
    public void _18_divisionByZero() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDivisionByZero(),
                Code.DIVISION_BY_ZERO );
    }
    @Test
    public void _19_portClosed() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mPortClosed(),
                Code.PORT_CLOSED );
    }
    @Test
    public void _20_expectedInputPort() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedInputPort(),
                Code.EXPECTED_INPUT_PORT );
    }
    @Test
    public void _21_expectedOutputPort() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedOutputPort(),
                Code.EXPECTED_OUTPUT_PORT );
    }
    @Test
    public void _22_ioError1() throws Exception
    {
        IOException iox = new FileNotFoundException( "not found" );

        validateMessageAndType(
                RuntimeX.mIoError( iox ),
                Code.IO_ERROR,
                str( iox.getMessage() ) );
    }
    @Test
    public void _23_duplicateFormal1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDuplicateFormal( s4 ),
                Code.DUPLICATE_FORMAL,
                s4 );
    }
    @Test
    public void _24_invalidFormals1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInvalidFormals( s4 ),
                Code.INVALID_FORMALS,
                s4 );
    }
    @Test
    public void _25_classNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mClassNotFound( what ),
                Code.CLASS_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void _26_fieldNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mFieldNotFound( what ),
                Code.FIELD_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void _27_methodNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mMethodNotFound( what ),
                Code.METHOD_NOT_FOUND,
                str( what ) );
    }
    @Test
    public void _27_methodNotFound2() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mMethodNotFound( what, Cons.create( i1, i2 ) ),
                Code.METHOD_NOT_FOUND,
                str( "what(1 2)" ) );
    }
    @Test
    public void _28_illegalAccess1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mIllegalAccess( what ),
                Code.ILLEGAL_ACCESS,
                str( what ) );
    }
    @Test
    public void _29_invocationException1() throws Exception
    {
        var executable = getClass().getMethods()[0];
        var throwable = new Exception( "testException" );

        validateMessageAndType(
                RuntimeX.mInvocationException( executable, throwable ),
                Code.INVOCATION_EXCEPTION,
                str( executable.toString() ),
                str( throwable.toString() ) );
    }
    @Test
    public void _30_cannotAcessInstance1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotAccessInstance(),
                Code.CANNOT_ACCESS_INSTANCE );
    }
    @Test
    public void _31_creationFailed1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mCreationFailed( what ),
                Code.CREATION_FAILED,
                str( what ) );
    }
    @Test
    public void _32_illegalArgument() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mIllegalArgument( what ),
                Code.ILLEGAL_ARGUMENT,
                str( what ) );
    }
    @Test
    public void _33_scanUnbalancedQuote() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedQuote(),
                Code.SCAN_UNBALANCED_QUOTE );
    }
    @Test
    public void _33_scanUnbalancedQuote2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedQuote(
                        new SourcePosition( 1, 2, null ) ),
                Code.SCAN_UNBALANCED_QUOTE,
                i1,
                i2 );
    }
    @Test
    public void _34_scanUnexpectedCharacter() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mScanUnexpectedChar(
                        new SourcePosition( 1, 2, null ),
                        what ),
                Code.SCAN_UNEXPECTED_CHAR,
                i1,
                i2,
                str( what ) );
    }
    @Test
    public void _35_error() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mError(),
                Code.ERROR );
    }
    @Test
    public void _36_parseExpected1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mParseExpected( Tk.Dot ),
                Code.PARSE_EXPECTED,
                str( Tk.Dot.toString() ) );
    }
    @Test
    public void _37_parseUnexpectedEof() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mParseUnexpectedEof(),
                Code.PARSE_UNEXPECTED_EOF );
    }
    @Test
    public void _38_parseUnexpected1() throws Exception
    {
        var token = new Token(
                Token.Tk.Dot,
                new SourcePosition( -1, -1, "_38_parseUnexpected1" ) );

        validateMessageAndType(
                RuntimeX.mParseUnexpected( token ),
                Code.PARSE_UNEXPECTED,
                str( token.toString() ) );
    }
    @Test
    public void _39_interrupted() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInterrupted(),
                Code.INTERRUPTED );
    }
    @Test
    public void _40_cannotModifyConstant() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotModifyConstant(),
                Code.CANNOT_MODIFY_CONSTANT );
    }
    @Test
    public void _40_cannotModifyConstant1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotModifyConstant( s1 ),
                Code.CANNOT_MODIFY_CONSTANT,
                s1 );
    }
    @Test
    public void _41_noProxy1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mNoProxy( what ),
                Code.NO_PROXY,
                str( what ) );
    }
    @Test
    public void _42_ProxyCannotInstantiate() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mProxyCannotInstantiate(),
                Code.PROXY_CANNOT_INSTANTIATE );
    }
    @Test
    public void _42_ProxyCannotInstantiate1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mProxyCannotInstantiate( what ),
                Code.PROXY_CANNOT_INSTANTIATE,
                str( what ) );
    }
    @Test
    public void _44_onlyInQuasiquoteContext() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mOnlyInQuasiquoteContext(),
                Code.ONLY_IN_QUASIQUOTE_CONTEXT );
    }
    @Test
    public void _45_radixNotSupported2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRadixNotSupported( 1, 2 ),
                Code.RADIX_NOT_SUPPORTED,
                i(1),
                i(2) );
    }
    @Test
    public void _46_duplicateElement_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDuplicateElement( s3 ),
                Code.DUPLICATE_ELEMENT,
                s3 );
    }
    @Test
    public void _47_expectedBinaryPort_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedBinaryPort( s3 ),
                Code.EXPECTED_BINARY_PORT,
                s3 );
    }
    @Test
    public void _48_expectedTextualPort_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedTextualPort( s3 ),
                Code.EXPECTED_TEXTUAL_PORT,
                s3 );
    }
    @Test
    public void _49_scanUnbalancedComment() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedComment( new SourcePosition( 1, 1, null ) ),
                Code.SCAN_UNBALANCED_COMMENT,
                i1,
                i1);
    }
    @Test
    public void _50_rangeExceeded() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRangeExceeded( i313, "> 256" ),
                Code.RANGE_EXCEEDED,
                i313,
                str( "> 256" ) );
    }
}
