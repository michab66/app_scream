/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.nio.file.Files;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.ScreamException.Code;

public class PortTest extends ScreamBaseTest
{
    private void _textualWrite( File file ) throws Exception
    {
        final var content =
                UUID.randomUUID().toString();

        file.delete();

        assertFalse( file.exists() );

        PortOut po = new PortOut( file.getPath() );

        assertFalse( po.isBinary() );
        assertFalse( po.isClosed() );

        po.write( content );

        // Exists but content is yet buffered.
        assertTrue( file.exists() );
        assertEquals( 0, file.length() );

        // Content is flushed.
        po.flush();
        assertEquals( content.length(), file.length() );

        po.close();
        assertTrue( po.isClosed() );

        assertTrue( file.exists() );

        var lines =  Files.readAllLines( file.toPath() );

        assertEquals( 1, lines.size() );
        assertEquals( content, lines.get( 0 ) );

        try {
            po.write( content );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.display( Cons.NIL );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.write( Cons.NIL );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.writeCharacter( (char)0 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.flush();
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
    }
    @Test
    public void textualWrite() throws Exception
    {
        withFile( this::_textualWrite );
    }

    private void _binaryWrite( File file ) throws Exception
    {
        final var content = new byte[] { 0,1,2,3,4,5 };

        file.delete();

        assertFalse( file.exists() );

        PortOutBinary po = new PortOutBinary( file.getPath() );

        assertTrue( po.isBinary() );
        assertFalse( po.isClosed() );

        po.write( content );

        // Exists but content is yet buffered.
        assertTrue( file.exists() );
        assertEquals( 0, file.length() );

        // Content is flushed.
        po.flush();
        assertEquals( content.length, file.length() );

        po.close();

        assertTrue( file.exists() );

        var lines =  Files.readAllBytes( file.toPath() );

        assertArrayEquals( content, lines );

        try {
            po.write( content );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.writeByte( 0 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.writeByte( i313 );
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
        try {
            po.flush();
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.PORT_CLOSED, rx.getCode() );
        }
    }
    @Test
    public void binaryWrite() throws Exception
    {
        withFile( this::_binaryWrite );
    }

}

