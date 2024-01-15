/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.SchemeString;

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
        expectFco(
                "(string-length \"Donald\")",
                "6");
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
    public void substring_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (substring "Donald" 1 4)
            """ );
        assertEqualq( SchemeString.makeEscaped( "ona" ), result );
    }
    @Test
    public void substring_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (substring "Donald" 4 4)
            """ );
        assertEqualq( SchemeString.makeEscaped( "" ), result );
    }

    @Test
    public void substring_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (let* (
              (donald-s "donald")
              (donald-len (string-length donald-s)))

              (substring donald-s 0 donald-len))
            """ );
        assertEqualq( SchemeString.makeEscaped( "donald" ), result );
    }

    @Test
    public void substring_4() throws Exception
    {
        var rx = expectError(
            """
            (substring "xxx" 2 8)
            """,
            Code.INDEX_OUT_OF_BOUNDS );
        assertEquals( 8L, rx.getArgument(0) );
    }

    @Test
    public void string_append_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huckle" "berry")
            """ );
        assertEqualq( SchemeString.makeEscaped( "Huckleberry" ), result );
    }

    @Test
    public void string_append_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huckle" "")
            """ );
        assertEqualq( str( "Huckle" ), result );
    }

    @Test
    public void string_append_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huck" "lebe" "rry")
            """ );
        assertEqualq( str( "Huckleberry" ), result );
    }

    @Test
    public void string_append_4() throws Exception
    {
        var rx = expectError(
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
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string=? "A" "A")
                """,
                bTrue );
        fcoc.accept(
                """
                (string=? "a" "A")
                """,
                bFalse );
    }

    @Test
    public void string_ci_equalq() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string-ci=? "A" "A")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci=? "a" "A")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci=? "A" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci=? "a" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci=? "a" "b")
                """,
                bFalse );
    }

    @Test
    public void string_lt() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string<? "a" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<? "b" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string<? "c" "a")
                """,
                bFalse );
        fcoc.accept(
                """
                (string<? "A" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<? "B" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_gt() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string>? "a" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>? "b" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>? "c" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string>? "A" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>? "B" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_gtci() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string-ci>? "a" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>? "b" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>? "c" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci>? "A" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>? "B" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_le() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string<=? "a" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<=? "b" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<=? "c" "a")
                """,
                bFalse );
        fcoc.accept(
                """
                (string<=? "A" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<=? "B" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string<=? "C" "a")
                """,
                bTrue );
    }

    @Test
    public void string_gle_ci() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string-ci<=? "a" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci<=? "b" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci<=? "c" "a")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci<=? "A" "c")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci<=? "B" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci<=? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_ge() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string>=? "a" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>=? "b" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string>=? "c" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string>=? "A" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>=? "B" "b")
                """,
                bFalse );
        fcoc.accept(
                """
                (string>=? "C" "a")
                """,
                bFalse );
    }

    @Test
    public void string_ge_ci() throws Exception
    {
        final var fcoc = expectFcoConsumer();

        fcoc.accept(
                """
                (string-ci>=? "a" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>=? "b" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci>=? "c" "a")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci>=? "A" "c")
                """,
                bFalse );
        fcoc.accept(
                """
                (string-ci>=? "B" "b")
                """,
                bTrue );
        fcoc.accept(
                """
                (string-ci>=? "C" "a")
                """,
                bTrue );
    }
}
