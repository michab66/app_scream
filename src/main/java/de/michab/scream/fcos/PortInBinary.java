/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;

import de.michab.scream.RuntimeX;

/**
 * A binary Scheme input port.
 * <p>
 * r7rs, 6.13.2 p58
 *
 * @author Michael Binz
 */
public class PortInBinary
    extends Port<InputStream>
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "input-port";

    private static final int NOT_PEEKED = -2;

    /**
     *
     */
    private int _peeked = NOT_PEEKED;

    /**
     * Create an input port.
     *
     * @param name A name to use for this port.
     * @param in The reader to use in this port.
     */
    public PortInBinary( String name, InputStream in )
    {
        super( name );

        stream( Objects.requireNonNull( in ) );
    }

    /**
     * Create a named port.  The passed name is used for opening the file.
     *
     * @param name The name of the file to open.
     * @param inout An enum value defining whether this is an input or output
     *        file.
     * @throws RuntimeX If an error occurred.
     */
    public PortInBinary( String name )
            throws RuntimeX
    {
        super( name );

        try
        {
            stream( new FileInputStream( name ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Checks whether a character is ready to be read from this port without
     * blocking.
     *
     * @return {@code True} if a character can be read without blocking.
     * @throws RuntimeX In case an error occurred.
     */
    public boolean byteReady()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            return stream().available() > 0;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Read a single character from the port without advancing the file position.
     * The next {@code peekCharacter()} or {@code readCharacter()}
     * operation will just return the same character.  In case there are no
     * characters available this call will block.
     *
     * @return The peeked character.
     * @throws RuntimeX In case an error occurred.
     * @see #readCharacter
     */
    public FirstClassObject peekByte()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        if ( _peeked == NOT_PEEKED )
        {
            try
            {
                _peeked = stream().read();
            }
            catch ( IOException e )
            {
                throw RuntimeX.mIoError( e );
            }
        }

        if ( _peeked == -1 )
            return EOF;

        return SchemeCharacter.createObject( _peeked );
    }

    /**
     * Read a character from this port.
     *
     * @return The character read or the end of file object.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject readByte()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        int c;
        if ( _peeked != NOT_PEEKED )
        {
            c = _peeked;
            _peeked = NOT_PEEKED;
        }
        else try
        {
            c = stream().read();
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }

        if ( c == -1 )
            return EOF;

        return SchemeInteger.createObject( c );
    }

    @Override
    public SchemeBoolean isBinary()
    {
        return SchemeBoolean.T;
    }
}
