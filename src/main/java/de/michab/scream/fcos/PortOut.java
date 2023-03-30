/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Objects;

import de.michab.scream.RuntimeX;

/**
 * Represents a scheme output-port
 *
 * r7rs, 6.13.3 p58
 *
 * @author Michael Binz
 */
public class PortOut
    extends Port<Writer>
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "output-port";


    /**
     * Create an output port.  The name that is passed is used for generating
     * correct file references in case of errors.
     *
     * @param name A name to use for this port.
     * @param out A writer that output gets written to.
     */
    public PortOut( String name, Writer out )
    {
        super( name );

        stream( Objects.requireNonNull( out ) );
    }

    /**
     * Create a named port.  The passed name is used for opening the file.
     *
     * @param name The name of the file to open.
     * @param inout An enum value defining whether this is an input or output
     *        file.
     * @throws RuntimeX If an error occurred.
     */
    public PortOut( String name )
            throws RuntimeX
    {
        super( name );

        try
        {
            stream( new BufferedWriter(
                    new FileWriter( name ) ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Write the passed object to the port.
     *
     * @param o The object to write.
     * @return The port.
     * @throws RuntimeX In case an error occurred.
     */
    public PortOut write( FirstClassObject o )
            throws RuntimeX
    {
        return write( toString( o ) );
    }

    /**
     * Write a string to the port.  This writes the external string
     * representation including quotes.
     *
     * @param s The string to write.
     * @return The port.
     * @throws RuntimeX In case an error occurred.
     */
    public PortOut write( String s )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            stream().write( s );
            return this;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Write a single character to this output port.
     *
     * @param c The character to write.
     * @return The port.
     * @throws RuntimeX In case something went wrong.
     */
    public PortOut writeCharacter( char c )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            stream().write( c );
            return this;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Write a human readable string for this FCO.  Human readable means that the
     * double quotes and quotes are not printed.
     *
     * @param o The object to display.
     * @throws RuntimeX In case an error occurred.
     */
    public PortOut display( FirstClassObject o )
            throws RuntimeX
    {
        if ( o instanceof SchemeString )
            return write( ((SchemeString)o).getValue() ).flush();

        if ( o instanceof SchemeCharacter )
            return write( "" + ((SchemeCharacter)o).asCharacter() ).flush();

        return write( o ).flush();
    }

    @Override
    public boolean isBinary()
    {
        return false;
    }

    /**
     * Flush the port.
     *
     * @return The port.
     * @throws RuntimeX
     */
    public PortOut flush()
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            stream().flush();
            return this;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }
}
