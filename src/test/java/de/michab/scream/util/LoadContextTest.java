package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.net.URL;

import org.junit.jupiter.api.Test;

public class LoadContextTest
{
    @Test
    public void urlJar() throws Exception
    {
        URL u = new URL(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/StreamCopier.class");

        LoadContext lc = new LoadContext( u );

        assertEquals(
                u.toExternalForm(),
                lc.toString() );
        assertEquals(
                "/com/elharo/io/StreamCopier.class",
                lc.getFile().getPath() );
    }
    @Test
    public void urlFile() throws Exception
    {
        URL u = new URL(
                "file:/Users/micbinz/git/github/app_scream/target/classes/de/michab/scream/extensions/basic.s");

        LoadContext lc = new LoadContext( u );

        assertEquals(
                u.toExternalForm(),
                lc.toString() );
    }

    @Test
    public void plainFile() throws Exception
    {
        String u =
                "/Users/micbinz/git/github/app_scream/target/classes/de/michab/scream/extensions/basic.s";

        LoadContext lc = new LoadContext( u );

        assertEquals(
                "file:" + u,
                lc.toString() );
        assertEquals(
                u,
                lc.getFile().getPath() );
    }

    @Test
    public void relativeFile() throws Exception
    {
        String a =
                "/de/michab/scream/extensions/a.s";
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );

        String b =
                "b.s";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcc = lcb.relate( lca );
        var file = lcc.getFile();
        assertEquals( "/de/michab/scream/extensions/b.s", file.getPath() );

        LoadContext lcd = new LoadContext( "/tmp/313.so" );
        assertTrue( lcd.isAbsolute() );

        LoadContext lcf = lcd.relate( lcc );
        file = lcf.getFile();
        assertEquals( "/tmp/313.so", file.getPath() );
    }

    @Test
    public void relativeFile2() throws Exception
    {
        String a =
                "file:/de/michab/scream/extensions/a.s";
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );
        String b =
                "b.s";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcc = lcb.relate( lca );
        var file = lcc.getFile();
        assertTrue( lca.isAbsolute() );
        assertEquals( "/de/michab/scream/extensions/b.s", file.getPath() );
    }

    @Test
    public void relativeJar() throws Exception
    {
        URL a = new URL(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/A.class");
        LoadContext lca = new LoadContext( a );
        assertTrue( lca.isAbsolute() );

        String b =
                "B.class";
        LoadContext lcb = new LoadContext( b );
        assertFalse( lcb.isAbsolute() );

        LoadContext lcbr = lcb.relate( lca );
        var file = lcbr.getFile();
        assertEquals( "/com/elharo/io/B.class", file.getPath() );
        assertEquals(
                "jar:http://www.oreilly.com/javaio.jar!/com/elharo/io/B.class",
                lcbr.toString() );
    }
}
