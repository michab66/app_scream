/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Objects;

import de.michab.scream.RuntimeX;
import de.michab.scream.frontend.SchemeParser;

/**
 * A Scheme textual input port.
 * <p>
 * r7rs, 6.13.2 p58
 *
 * @author Michael Binz
 */
public class PortIn
    extends Port<BufferedReader>
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "input-port";

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

        Objects.requireNonNull( in );

        if ( ! BufferedReader.class.isAssignableFrom( in.getClass() ) )
            in = new BufferedReader( in );

        stream( (BufferedReader)in );
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
            stream( new BufferedReader(
                        new InputStreamReader(
                            new FileInputStream( name ) ) ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
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
            _parser = new SchemeParser( stream(), name() );

        return _parser.getExpression();
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
            return stream().ready();
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
            c = stream().read();
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }

        if ( c == -1 )
            return EOF;

        return SchemeCharacter.createObject( c );
    }

    @Override
    public Bool isBinary()
    {
        return Bool.F;
    }

    /**
     *
     * @return
     */
    public FirstClassObject readLine()
        throws RuntimeX
    {
        try
        {
            var result = stream().readLine();

            if ( result == null )
                return EOF;

            return SchemeString.make( result );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    public FirstClassObject readString( int k )
            throws RuntimeX
    {
        try
        {
            var buffer = new char[k];

            var actual = stream().read( buffer, 0, k );

            if ( actual == -1 )
                return EOF;

            return SchemeString.make(
                    new String(
                            buffer,
                            0,
                            actual ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }
}
