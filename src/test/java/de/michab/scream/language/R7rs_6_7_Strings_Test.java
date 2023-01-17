/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.language;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;
import de.michab.scream.fcos.SchemeBoolean;
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
        var result = scriptEngine().evalFco(
            """
            (string? "Donald")
            """ );
        assertEquals( SchemeBoolean.T, result );
    }

    @Test
    public void stringp_1b() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (or
              (string? ())
              (string? 'donald)
              (string? 313)
              (string? 3.1415)
              (string? (cons 1 2)))
            """ );
        assertEquals( SchemeBoolean.F, result );
    }

    @Test
    public void substring_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (substring "Donald" 1 4)
            """ );
        assertEqualq( new SchemeString( "ona" ), result );
    }
    @Test
    public void substring_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (substring "Donald" 4 4)
            """ );
        assertEqualq( new SchemeString( "" ), result );
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
        assertEqualq( new SchemeString( "donald" ), result );
    }
    @Test
    public void substring_4() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
            """
            (substring "xxx" 2 8)
            """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.INDEX_OUT_OF_BOUNDS, rx.getCode() );
            assertEquals( 8L, rx.getArgument(0) );
        }
    }
    @Test
    public void string_append_1() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huckle" "berry")
            """ );
        assertEqualq( new SchemeString( "Huckleberry" ), result );
    }
    @Test
    public void string_append_2() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huckle" "")
            """ );
        assertEqualq( new SchemeString( "Huckle" ), result );
    }
    @Test
    public void string_append_3() throws Exception
    {
        var result = scriptEngine().evalFco(
            """
            (string-append "Huck" "lebe" "rry")
            """ );
        assertEqualq( new SchemeString( "Huckleberry" ), result );
    }
    @Test
    public void string_append_4() throws Exception
    {
        try
        {
            scriptEngine().evalFco(
            """
            (string-append "Huckle" ())
            """ );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TYPE_ERROR, rx.getCode() );
            assertEquals( "string", rx.getArgument(0) );
            assertEquals( "NIL", rx.getArgument(1) );
            assertEquals( "2", rx.getArgument(2) );
        }
    }
}
