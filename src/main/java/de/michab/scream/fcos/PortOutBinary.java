/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Objects;

import de.michab.scream.RuntimeX;

/**
 * A binary Scheme output-port.
 *
 * r7rs, 6.13.3 p58
 *
 * @author Michael Binz
 */
public class PortOutBinary
    extends Port<OutputStream>
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
    public PortOutBinary( String name, OutputStream out )
    {
        super( name );

        stream( Objects.requireNonNull( out ) );
    }

    /**
     * Create a named port.  The passed name is used for opening the file.
     * The resulting port is buffered.
     *
     * @param name The name of the file to open.
     * @throws RuntimeX If an error occurred.
     */
    public PortOutBinary( String name )
            throws RuntimeX
    {
        super( name );

        try
        {
            stream( new BufferedOutputStream(
                    new FileOutputStream( name ) ) );
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Write a single byte to this output port.
     *
     * @param c The character to write.
     * @throws RuntimeX In case something went wrong.
     */
    public PortOutBinary writeByte( SchemeInteger c )
            throws RuntimeX
    {
        return writeByte( (int)c.asLong() );
    }

    /**
     * Write a single byte to this output port. Note that only
     * the lower eight bits of the passed integer are written.
     *
     * @param b The byte to write.
     * @throws RuntimeX In case something went wrong.
     */
    public PortOutBinary writeByte( int b )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            stream().write( b );
            return this;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Write an array of bytes.
     *
     * @param b The bytes to write.
     * @throws RuntimeX In case something went wrong.
     */
    public PortOutBinary write( byte[] b )
            throws RuntimeX
    {
        if ( isClosed() )
            throw RuntimeX.mPortClosed();

        try
        {
            stream().write( b );
            return this;
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Flush the output port.
     *
     * @throws RuntimeX In case something went wrong.
     */
    public PortOutBinary flush()
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

    @Override
    public Bool isBinary()
    {
        return Bool.T;
    }
}
