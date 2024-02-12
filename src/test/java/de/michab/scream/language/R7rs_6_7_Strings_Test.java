/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.smack.util.StringUtil;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * rsr7 6.7 Strings, p45
 *
 * @author MICBINZ
 */
public class R7rs_6_7_Strings_Test extends ScreamBaseTest
{
    /**
     * p46
     */
    @Test
    public void stringp_1() throws Exception
    {
        expectFco(
            """
            (string? "Donald")
            """,
            bTrue );
    }

    @Test
    public void stringp_1b() throws Exception
    {
        expectFco(
            """
            (or
              (string? ())
              (string? 'donald)
              (string? 313)
              (string? 3.1415)
              (string? (cons 1 2)))
            """,
            bFalse );
    }

    @Test
    public void make_string_1() throws Exception
    {
        expectFco(
            """
            (make-string 8)
            """,
            """
            "        "
            """);
    }

    @Test
    public void make_string_2() throws Exception
    {
        expectFco(
                """
                (make-string 8 #\\ß)
                """,
                """
                "ßßßßßßßß"
                """);
    }

    /**
     *  (string char ...) library procedure; r5rs 30
     * @throws Exception
     */
    @Test
    public void string_char() throws Exception
    {
        expectFco(
"""
                (string #\\D #\\o #\\n #\\a #\\l #\\d)
""",
                str( "Donald" ) );
    }


    @Test
    public void string_length() throws Exception
    {
        var t = makeTester();

        t.expectFco(
                "(string-length \"Donald\")",
                "6");
        t.expectFco(
                "(string-length \"\")",
                "0");
        t.expectError(
                "(string-length 'string)",
                Code.TYPE_ERROR );
    }

    @Test
    public void string_ref() throws Exception
    {
        expectFco(
                "(string-ref \"Donald\" 4)",
                "#\\l");
    }

    @Test
    public void string_set() throws Exception
    {
        expectFco(
"""
                (define donald "Tonald")
                (string-set! donald 0 #\\D)
                donald
""",
                "\"Donald\"");
    }
    @Test
    public void string_set_constant() throws Exception
    {
        expectError(
"""
                (define donald '"Tonald")
                (string-set! donald 0 #\\D)
                donald
""",
                Code.CANNOT_MODIFY_CONSTANT );
    }


    // ----------

    @Test
    public void substring() throws Exception
    {
        var t = makeTester();

        t.expectFco(
            "(substring \"Donald\" 1 4)",
            str( "ona" ) );
        t.expectFco(
                "(substring \"Donald\" 4 4)",
                str( StringUtil.EMPTY_STRING ) );
        t.expectFco(
            """
            (let* (
              (donald-s "donald")
              (donald-len (string-length donald-s)))

              (substring donald-s 0 donald-len))
            """,
            str( "donald" ) );

        var rx = t.expectError(
            """
            (substring "xxx" 2 8)
            """,
            Code.INDEX_OUT_OF_BOUNDS );
        assertEquals( 8L, rx.getArgument(0) );

        // MIT Scheme examples
        t.expectFco(
                "(substring \"\" 0 0)",
                str( StringUtil.EMPTY_STRING ) );
        t.expectFco(
                "(substring \"arduous\" 2 5)",
                "\"duo\"" );
        t.expectError(
                "(substring \"arduous\" 2 8)",
                Code.INDEX_OUT_OF_BOUNDS ); // error--> 8 not in correct range
    }

    @Test
    public void string_append() throws Exception
    {
        var t = makeTester();

        t.expectFco(
            """
            (string-append "Huckle" "berry")
            """,
            str( "Huckleberry" ) );
        t.expectFco(
            """
            (string-append "Huckle" "")
            """,
            str( "Huckle" ) );
        t.expectFco(
            """
            (string-append "Huck" "lebe" "rry")
            """,
            str( "Huckleberry" ) );
        var rx = t.expectError(
            """
            (string-append "Huckle" ())
            """,
            Code.TYPE_ERROR );
        assertEquals( "string", rx.getArgument(0) );
        assertEquals( "NIL", rx.getArgument(1) );
        assertEquals( "2", rx.getArgument(2) );
    }

    @Test
    public void string_to_list() throws Exception
    {
        expectFco(
            """
            (string->list "Donald Duck")
            """,
            "(#\\D #\\o #\\n #\\a #\\l #\\d #\\space #\\D #\\u #\\c #\\k)" );
    }

    @Test
    public void list_to_string() throws Exception
    {
        expectFco(
            """
            (list->string '(#\\D #\\o #\\n #\\a #\\l #\\d #\\space #\\D #\\u #\\c #\\k))
            """,
            "\"Donald Duck\"" );
    }

    @Test
    public void string_copy() throws Exception
    {
        expectFco(
                """
                (define schlawinie "Schlawinie Kümmelbein")
                (define schlawinia (string-copy schlawinie))
                (string-set! schlawinia 9 #\\a)

                (and
                  (equal? "Schlawinie Kümmelbein" schlawinie)
                  (equal? "Schlawinia Kümmelbein" schlawinia))
                """,
                bTrue );
    }

    @Test
    public void string_fill() throws Exception
    {
        expectFco(
                """
                (define schlawinia "Schlawinia Kümmelbein")
                (string-fill! schlawinia #\\x)
                schlawinia
                """,
                "\"xxxxxxxxxxxxxxxxxxxxx\"" );
    }

    @Test
    public void string_equalq() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string=? "A" "A")
                """,
                bTrue );
        t.expectFco(
                """
                (string=? "a" "A")
                """,
                bFalse );
    }

    @Test
    public void string_ci_equalq() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string-ci=? "A" "A")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci=? "a" "A")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci=? "A" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci=? "a" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci=? "a" "b")
                """,
                bFalse );
    }

    @Test
    public void string_lt() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string<? "a" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string<? "b" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string<? "c" "a")
                """,
                bFalse );
        t.expectFco(
                """
                (string<? "A" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string<? "B" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string<? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_gt() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string>? "a" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string>? "b" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string>? "c" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string>? "A" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string>? "B" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string>? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_gtci() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string-ci>? "a" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>? "b" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>? "c" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci>? "A" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>? "B" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_le() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string<=? "a" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string<=? "b" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string<=? "c" "a")
                """,
                bFalse );
        t.expectFco(
                """
                (string<=? "A" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string<=? "B" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string<=? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_gle_ci() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string-ci<=? "a" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci<=? "b" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci<=? "c" "a")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci<=? "A" "c")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci<=? "B" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci<=? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_ge() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string>=? "a" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string>=? "b" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string>=? "c" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string>=? "A" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string>=? "B" "b")
                """,
                bFalse );
        t.expectFco(
                """
                (string>=? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_ge_ci() throws Exception
    {
        final var t = makeTester();

        t.expectFco(
                """
                (string-ci>=? "a" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>=? "b" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci>=? "c" "a")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci>=? "A" "c")
                """,
                bFalse );
        t.expectFco(
                """
                (string-ci>=? "B" "b")
                """,
                bTrue );
        t.expectFco(
                """
                (string-ci>=? "C" "a")
                """,
                bTrue );
    }
}
