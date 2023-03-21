/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import javax.script.ScriptEngineFactory;

import org.junit.jupiter.api.Test;

public class ScreamTest extends ScreamBaseTest
{
    @Test
    public void engineNameTest() throws Exception
    {
        ScriptEngineFactory sef = new Scream();

        var v = sef.getEngineName();
        assertEquals( "scream", v );
    }

    @Test
    public void extensionsTest() throws Exception
    {
        ScriptEngineFactory sef = new Scream();

        var v = sef.getExtensions();
        assertEquals( 2, v.size() );
        assertTrue( v.contains( "s" ) );
        assertTrue( v.contains( "scm" ) );
    }

    @Test
    public void namesTest() throws Exception
    {
        ScriptEngineFactory sef = new Scream();

        var v = sef.getNames();
        assertEquals( 2, v.size() );
        assertTrue( v.contains( "scheme" ) );
        assertTrue( v.contains( "scream" ) );
    }

    @Test
    public void languageNameTest() throws Exception
    {
        ScriptEngineFactory sef = new Scream();

        var v = sef.getLanguageName();
        assertEquals( "Scheme", v );
    }

    @Test
    public void languageVersionTest() throws Exception
    {
        ScriptEngineFactory sef = new Scream();

        var v = sef.getLanguageVersion();
        assertEquals( "7", v );
    }
}
