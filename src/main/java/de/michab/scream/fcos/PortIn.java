/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

import org.smack.util.FileUtil;

import de.michab.scream.RuntimeX;
import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

/**
 * Represents a scheme port object.  Ports represent input and output devices.
 * To Scheme, an input port is a Scheme object that can deliver characters
 * upon command, while an output port is a Scheme object that can accept
 * characters.
 * <p>
 * See r7rs, 6.13 Input and output for details.
 *
 * @author Michael Binz
 */
public class PortIn
    extends Port
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "input-port";

    /**
     * The object representing an input port.
     */
    private Reader _inPort = null;

    /**
     * The parser used by the read method.
     */
    private SchemeParser _parser = null;

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
    public PortIn( String name, Reader in )
    {
        super( name );
        _inPort = in;
    }

    /**
     * Create a named port.  The passed name is used for opening the file.
     *
     * @param name The name of the file to open.
     * @param inout An enum value defining whether this is an input or output
     *        file.
     * @throws RuntimeX If an error occurred.
     */
    public PortIn( String name )
            throws RuntimeX
    {
        super( name );

        try
        {
            _inPort = new InputStreamReader(
                    new FileInputStream( name ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Return a port's string representation.
     *
     * @return A string representation for this port.
     */
    @Override
    public String toString()
    {
        return String.format(
                "<input-port '%s' %s>",
                name(),
                type() );
    }

    /**
     * Tests if a port is closed.
     *
     * @return {@code true} if the port is closed, {@code false} if the
     * port is open.
     */
    @Override
    public boolean isClosed()
    {
        return _inPort == null;
    }

    /**
     * Read a single first class object from this port.  After the read the port
     * is positioned on the first character after this data element.
     *
     * @return The next datum encountered on the input port.
     * @throws RuntimeX In case of an error.
     */
    public FirstClassObject read()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        // In case no parser exists...
        if ( null == _parser )
            // ...create one.
            _parser = new SchemeParser( _inPort );

        try
        {
            return _parser.getExpression();
        }
        catch ( FrontendX e )
        {
            e.setFilename( name() );
            throw e;
        }
    }

    /**
     * Checks whether a character is ready to be read from this port without
     * blocking.
     *
     * @return {@code True} if a character can be read without blocking.
     * @throws RuntimeX In case an error occurred.
     */
    public boolean charReady()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            return _inPort.ready();
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
    public FirstClassObject peekCharacter()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        if ( _peeked == NOT_PEEKED )
        {
            try
            {
                _peeked = _inPort.read();
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
    public FirstClassObject readCharacter()
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
            c = _inPort.read();
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }

        if ( c == -1 )
            return EOF;
        else
            return SchemeCharacter.createObject( c );
    }

    /**
     * Closes this port.  This is also done from the finalizer.
     */
    @Override
    public void close()
    {
        FileUtil.forceClose( _inPort );
        _inPort = null;
    }

    /**
     * Convert this object into the Java type system.  For input ports returns a
     * reader, for output ports a writer.
     *
     * @return The corresponding Java type for this object.
     */
    @Override
    public Object toJava()
    {
        return _inPort;
    }
}
