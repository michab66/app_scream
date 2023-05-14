/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
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
import de.michab.scream.fcos.Port;
import de.michab.scream.fcos.SchemeDouble;
import de.michab.scream.frontend.Token;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.util.ErrorMessages;

public class RuntimeXTest extends ScreamBaseTest
{
    @Test
    public void blankError() throws Exception
    {
        try
        {
            new RuntimeX( "" );
            fail();
        }
        catch ( IllegalArgumentException expected )
        {
        }
    }

    @Test
    public void paramMismatch() throws Exception
    {
        // BAD_BINDING is only defined by BAD_BINDING_2,  this
        // tests the fallback to BAD_BINDING.
        assertFalse(
                ErrorMessages.map.containsKey( Code.BAD_BINDING.toString() )
        )
        ;
        var se = new RuntimeX( Code.BAD_BINDING, "unknown" );

        System.out.println( se.getMessage() );
        assertEquals(
                Code.BAD_BINDING.id(),
                se.getId() );
        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );
    }

    @Test
    public void internalError() throws Exception
    {
        var se = new RuntimeX( Code.INTERNAL_ERROR.name() );

        assertEquals(
                -1,
                se.getId() );
        assertEquals(
                Code.INTERNAL_ERROR,
                se.getCode() );
    }

    @Test
    public void _16_badBinding() throws Exception
    {
        var se = new RuntimeX( Code.BAD_BINDING.name(), "a1", "a2" );

        assertEquals(
                Code.BAD_BINDING.id(),
                se.getId() );
        assertEquals(
                Code.BAD_BINDING,
                se.getCode() );

        assertTrue( se.getMessage().contains( "a1" ) );
        assertTrue( se.getMessage().contains( "a2" ) );
    }

    @Test
    public void unknownCode() throws Exception
    {
        var rx = new RuntimeX( "duck" );
        assertEquals( Code.ERROR, rx.getCode() );
        assertEquals( "35 : duck", rx.getMessage() );
    }

    @Test
    public void unknownName() throws Exception
    {
            var sex = new RuntimeX( ".UNKNOWN", 1, 2, 3 );
            assertEquals( Code.ERROR, sex.getCode() );
            assertEquals( "35 : .UNKNOWN 1 2 3", sex.getMessage() );
    }

//    @Test
//    public void nameInternal() throws Exception
//    {
//        var se = new ScreamException( "INTERNAL_ERROR" );
//        assertEquals( -1, se.getId() );
//        assertEquals( Code.INTERNAL_ERROR, se.getCode() );
//    }


    private RuntimeX validateMessageAndType( RuntimeX x, Code c, int id, Object ... args )
    {
        var msg = x.getMessage();
        assertNotNull( msg );
        assertEquals(
                c,
                x.getCode() );
        assertEquals(
                id,
                x.getId() );
        if ( args.length != 0  )
            assertArrayEquals( args, x.getArguments() );
        return x;
    }

    @Test
    public void _m1_internalError() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInternalError(),
                Code.INTERNAL_ERROR,
                -1 );
    }
    @Test
    public void _m1_internalError1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInternalError( "313" ),
                Code.INTERNAL_ERROR,
                -1,
                "313" );
    }
    @Test
    public void _0_notImplemented() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotImplemented(),
                Code.NOT_IMPLEMENTED,
                0 );
    }
    @Test
    public void _0_notImplemented1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotImplemented( "donald" ),
                Code.NOT_IMPLEMENTED,
                0,
                "donald" );
    }
    @Test
    public void _1_symbolNotDefined() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSymbolNotDefined( s313 ),
                Code.SYMBOL_NOT_DEFINED,
                1,
                s313 );
    }
    @Test
    public void _2_symbolNotAssignable() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSymbolNotAssignable( s313 ),
                Code.SYMBOL_NOT_ASSIGNABLE,
                2,
                s313 );
    }
    @Test
    public void _3_tooManySubexpressions() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManySubexpressions(),
                Code.TOO_MANY_SUBEXPRESSIONS,
                3 );
    }
    @Test
    public void _3_tooManySubexpressions1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManySubexpressions( s313 ),
                Code.TOO_MANY_SUBEXPRESSIONS,
                3,
                s313 );
    }
    @Test
    public void _4_syntaxError() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSyntaxError(),
                Code.SYNTAX_ERROR,
                4 );
    }
    @Test
    public void _4_syntaxError1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mSyntaxError( s313 ),
                Code.SYNTAX_ERROR,
                4,
                s313 );
    }
    @Test
    public void _5_defineError() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDefineError(),
                Code.DEFINE_ERROR,
                5 );
    }
    @Test
    public void _6_expectedProperList() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedProperList(),
                Code.EXPECTED_PROPER_LIST,
                6 );
    }
    @Test
    public void _6_expectedProperList1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedProperList( s313 ),
                Code.EXPECTED_PROPER_LIST,
                6,
                s313 );
    }
    @Test
    public void _7_indexOutOfBounds() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mIndexOutOfBounds( 7 ),
                Code.INDEX_OUT_OF_BOUNDS,
                7,
                7L );
    }
    @Test
    public void _8_calledNonProcedural() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCalledNonProcedural( s313 ),
                Code.CALLED_NON_PROCEDURAL,
                8,
                s313.toJava() );
    }
    @Test
    public void _9_invalidAssocList() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInvalidAssocList( s313 ),
                Code.INVALID_ASSOC_LIST,
                9,
                s313 );
    }
    @Test
    public void _10_carFailed() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCarFailed( s313 ),
                Code.CAR_FAILED,
                10,
                s313 );
    }
    @Test
    public void _11_typeError2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, SchemeDouble.class ),
                Code.TYPE_ERROR,
                11,
                Cons.class,
                SchemeDouble.class );
    }
    @Test
    public void _11_typeError3() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTypeError( Cons.class, Port.class, 313 ),
                Code.TYPE_ERROR,
                11,
                Cons.class,
                Port.class,
                313 );
    }
    @Test
    public void _12_notEnoughArguments() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotEnoughArguments( 313 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                12,
                313L );
    }
    @Test
    public void _12_notEnoughArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mNotEnoughArguments( 313, 314 ),
                Code.NOT_ENOUGH_ARGUMENTS,
                12,
                313L,
                314L );
    }
    @Test
    public void _13_tooManyArguments1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManyArguments( 313 ),
                Code.TOO_MANY_ARGUMENTS,
                13,
                313L );
    }
    @Test
    public void _13_tooManyArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mTooManyArguments( 313, 314 ),
                Code.TOO_MANY_ARGUMENTS,
                13,
                313L,
                314L );
    }
    @Test
    public void _14_wrongNumberOfArguments1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mWrongNumberOfArguments( 313 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                14,
                313L );
    }
    @Test
    public void _14_wrongNumberOfArguments2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mWrongNumberOfArguments( 313, 314 ),
                Code.WRONG_NUMBER_OF_ARGUMENTS,
                14,
                313L,
                314L );
    }
    @Test
    public void _15_requiresEquivalentConsLen() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRequiresEqivalentConsLength(),
                Code.REQUIRES_EQUIVALENT_CONS_LEN,
                15 );
    }
    @Test
    public void _16_badBinding_2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mBadBinding( s313, s1 ),
                Code.BAD_BINDING,
                16,
                s313,
                s1 );
    }
    @Test
    public void _17_badClause_3() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mBadClause( s2 ),
                Code.BAD_CLAUSE,
                17,
                s2 );
    }
    @Test
    public void _18_divisionByZero() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDivisionByZero(),
                Code.DIVISION_BY_ZERO,
                18 );
    }
    @Test
    public void _19_portClosed() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mPortClosed(),
                Code.PORT_CLOSED,
                19 );
    }
    @Test
    public void _20_expectedInputPort() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedInputPort(),
                Code.EXPECTED_INPUT_PORT,
                20 );
    }
    @Test
    public void _21_expectedOutputPort() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedOutputPort(),
                Code.EXPECTED_OUTPUT_PORT,
                21 );
    }
    @Test
    public void _22_ioError1() throws Exception
    {
        IOException iox = new FileNotFoundException( "not found" );

        validateMessageAndType(
                RuntimeX.mIoError( iox ),
                Code.IO_ERROR,
                22,
                iox.getMessage() );
    }
    @Test
    public void _23_duplicateFormal1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDuplicateFormal( s4 ),
                Code.DUPLICATE_FORMAL,
                23,
                s4 );
    }
    @Test
    public void _24_invalidFormals1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInvalidFormals( s4 ),
                Code.INVALID_FORMALS,
                24,
                s4 );
    }
    @Test
    public void _25_classNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mClassNotFound( what ),
                Code.CLASS_NOT_FOUND,
                25,
                what );
    }
    @Test
    public void _26_fieldNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mFieldNotFound( what ),
                Code.FIELD_NOT_FOUND,
                26,
                what );
    }
    @Test
    public void _27_methodNotFound1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mMethodNotFound( what ),
                Code.METHOD_NOT_FOUND,
                27,
                what );
    }
    @Test
    public void _27_methodNotFound2() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mMethodNotFound( what, Cons.create( i1, i2 ) ),
                Code.METHOD_NOT_FOUND,
                27,
                "what(1 2)" );
    }
    @Test
    public void _28_illegalAccess1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mIllegalAccess( what ),
                Code.ILLEGAL_ACCESS,
                28,
                what );
    }
    @Test
    public void _29_invocationException1() throws Exception
    {
        var executable = getClass().getMethods()[0];
        var throwable = new Exception( "testException" );

        validateMessageAndType(
                RuntimeX.mInvocationException( executable, throwable ),
                Code.INVOCATION_EXCEPTION,
                29,
                executable,
                throwable.toString() );
    }
    @Test
    public void _30_cannotAcessInstance1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotAccessInstance(),
                Code.CANNOT_ACCESS_INSTANCE,
                30 );
    }
    @Test
    public void _31_creationFailed1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mCreationFailed( what ),
                Code.CREATION_FAILED,
                31,
                what );
    }
    @Test
    public void _32_illegalArgument() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mIllegalArgument( what ),
                Code.ILLEGAL_ARGUMENT,
                32,
                what );
    }
    @Test
    public void _33_scanUnbalancedQuote() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedQuote(),
                Code.SCAN_UNBALANCED_QUOTE,
                33 );
    }
    @Test
    public void _33_scanUnbalancedQuote2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedQuote( 1, 2 ),
                Code.SCAN_UNBALANCED_QUOTE,
                33,
                1,
                2 );
    }
    @Test
    public void _34_scanUnexpectedCharacter() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mScanUnexpectedCharacter( 1, 2, what ),
                Code.SCAN_UNEXPECTED_CHAR,
                34,
                1,
                2,
                what );
    }
    @Test
    public void _35_error() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mError(),
                Code.ERROR,
                35 );
    }
    @Test
    public void _36_parseExpected1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mParseExpected( Tk.Dot ),
                Code.PARSE_EXPECTED,
                36,
                Tk.Dot );
    }
    @Test
    public void _37_parseUnexpectedEof() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mParseUnexpectedEof(),
                Code.PARSE_UNEXPECTED_EOF,
                37 );
    }
    @Test
    public void _38_parseUnexpected1() throws Exception
    {
        var token = Token.createToken( Token.Tk.Dot );

        validateMessageAndType(
                RuntimeX.mParseUnexpected( token ),
                Code.PARSE_UNEXPECTED,
                38,
                token );
    }
    @Test
    public void _39_interrupted() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mInterrupted(),
                Code.INTERRUPTED,
                39 );
    }
    @Test
    public void _40_cannotModifyConstant() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotModifyConstant(),
                Code.CANNOT_MODIFY_CONSTANT,
                40 );
    }
    @Test
    public void _40_cannotModifyConstant1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mCannotModifyConstant( s1 ),
                Code.CANNOT_MODIFY_CONSTANT,
                40,
                s1 );
    }
    @Test
    public void _41_noProxy1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mNoProxy( what ),
                Code.NO_PROXY,
                41,
                what );
    }
    @Test
    public void _42_ProxyCannotInstantiate() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mProxyCannotInstantiate(),
                Code.PROXY_CANNOT_INSTANTIATE,
                42 );
    }
    @Test
    public void _42_ProxyCannotInstantiate1() throws Exception
    {
        var what = "what";

        validateMessageAndType(
                RuntimeX.mProxyCannotInstantiate( what ),
                Code.PROXY_CANNOT_INSTANTIATE,
                42,
                what );
    }
    @Test
    public void _43_testFailed2() throws Exception
    {
        var a = "a";
        var b = "b";

        validateMessageAndType(
                RuntimeX.mTestFailed( a, b ),
                Code.TEST_FAILED,
                43,
                a,
                b );
    }
    @Test
    public void _44_onlyInQuasiquoteContext() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mOnlyInQuasiquoteContext(),
                Code.ONLY_IN_QUASIQUOTE_CONTEXT,
                44 );
    }
    @Test
    public void _45_radixNotSupported2() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRadixNotSupported( 1, 2 ),
                Code.RADIX_NOT_SUPPORTED,
                45,
                1,
                2 );
    }
    @Test
    public void _46_duplicateElement_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mDuplicateElement( s3 ),
                Code.DUPLICATE_ELEMENT,
                46,
                s3 );
    }
    @Test
    public void _47_expectedBinaryPort_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedBinaryPort( s3 ),
                Code.EXPECTED_BINARY_PORT,
                47,
                s3 );
    }
    @Test
    public void _48_expectedTextualPort_1() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mExpectedTextualPort( s3 ),
                Code.EXPECTED_TEXTUAL_PORT,
                48,
                s3 );
    }
    @Test
    public void _49_scanUnbalancedComment() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mScanUnbalancedComment( 1, 1 ),
                Code.SCAN_UNBALANCED_COMMENT,
                49,
                1,
                1);
    }
    @Test
    public void _50_rangeExceeded() throws Exception
    {
        validateMessageAndType(
                RuntimeX.mRangeExceeded( i313, "> 256" ),
                Code.RANGE_EXCEEDED,
                50,
                i313,
                "> 256" );
    }
}
