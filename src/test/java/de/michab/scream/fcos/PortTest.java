/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Closeable;
import java.io.File;
import java.nio.file.Files;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;

public class PortTest extends ScreamBaseTest
{
    private Closeable _textualReadWrite( File file ) throws Exception
    {
        file.delete();

        assertFalse( file.exists() );

        PortOut po = new PortOut( file.getPath() );
        po.write( "313", true );
        po.close();

        assertTrue( file.exists() );

        var lines =  Files.readAllLines( file.toPath() );

        assertEquals( 1, lines.size() );
        assertEquals( "313", lines.get( 0 ) );

        return null;
    }
    @Test
    public void textualReadWrite() throws Exception
    {
        withFile( this::_textualReadWrite );
    }
}

