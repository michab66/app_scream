package de.michab.scream.util;

import static org.junit.jupiter.api.Assertions.assertEquals;

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
    }
}
