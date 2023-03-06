/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

import org.smack.util.FileUtil;

import de.michab.scream.RuntimeX;

/**
 * Represents a scheme output-port
 *
 * @author Michael Binz
 */
public class PortOut
    extends Port
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "output-port";

    private Writer _outPort;

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
        _outPort = out;
    }
    public PortOut( String name, Writer out, boolean finalize )
    {
        super( name, finalize );
        _outPort = out;
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
            _outPort = new PrintWriter(
                    new FileOutputStream( name ) );
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
                "<output-port '%s' %s>",
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
        return _outPort == null;
    }

    /**
     * Write the passed object to the port.
     *
     * @param o The object to write.
     * @param flush {@code True} results in the port being flushed after the
     *        data is written.
     * @throws RuntimeX In case an error ocurred.
     */
    public void write( FirstClassObject o, boolean flush )
            throws RuntimeX
    {
        write( toString( o ), flush );
    }

    /**
     * Write the passed object to the port.
     *
     * @param o The object to write.
     * @throws RuntimeX In case an error occurred.
     */
    public void write( FirstClassObject o )
            throws RuntimeX
    {
        write( o, false );
    }

    /**
     * Write a string to the port.  This writes the external string
     * representation including quotes.
     *
     * @param s The string to write.
     * @param flush If {@code true} then port is flushed after the write.
     * @throws RuntimeX In case an error occurred.
     */
    void write( String s, boolean flush )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            _outPort.write( s );
            _outPort.write( ' ' );
            if ( flush )
                _outPort.flush();
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
     * @throws RuntimeX In case something went wrong.
     */
    public void writeCharacter( char c )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            _outPort.write( c );
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
    public void display( FirstClassObject o )
            throws RuntimeX
    {
        if ( o instanceof SchemeString )
            write( ((SchemeString)o).getValue(), true );
        else if ( o instanceof SchemeCharacter )
            write( "" + ((SchemeCharacter)o).asCharacter(), true );
        else
            write( o );
    }

    /**
     * Closes this port.  This is also done from the finalizer.
     */
    @Override
    public void close()
    {
        FileUtil.forceClose( _outPort );
        _outPort = null;
    }

    /**
     * Convert this object into the Java type system.
     *
     * @return The corresponding Java type for this object.
     */
    @Override
    public Object toJava()
    {
        return _outPort;
    }
}
