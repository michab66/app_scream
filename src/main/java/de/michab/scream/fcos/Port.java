/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.Closeable;
import java.io.IOException;

import org.smack.util.JavaUtil;

import de.michab.scream.RuntimeX;

/**
 * A port.  A port can be set to constant to prevent it
 * from being closed.  This can be used for the standard
 * stdin/stderr/stdout streams.
 *
 * <p>
 * See 'r7rs, 6.13 Input and output' for details.
 *
 * @author Michael Binz
 */
public abstract class Port<T extends Closeable>
    extends
        FirstClassObject
    implements
        AutoCloseable
{
    public static final String TYPE_NAME = "port";

    /**
     * An object representing EOF.
     */
    public static final Symbol EOF = Symbol.createObject( "EOF" );

    private final String _name;

    protected T _file;

    /**
     * Create a port.
     *
     * @param name The port's name.
     */
    protected Port( String name )
    {
        _name = name;
    }

    public String name()
    {
        return _name;
    }

    /**
     * Tests if the port is closed.
     *
     * @return {@code true} if the port is closed, {@code false} if the
     * port is open.
     */
    public final boolean isClosed()
    {
        return _file == null;
    }

    public abstract boolean isBinary();

    /**
     * Closes the port.
     */
    @Override
    public final void close() throws RuntimeX
    {
        if ( isClosed() )
            return;
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        try
        {
            _file.close();
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
        finally
        {
            _file = null;
        }
    }

    /**
     * Finalize the object.
     *
     * @throws Throwable In case of errors.
     * @see java.lang.Object#finalize
     */
    @Override
    protected void finalize()
            throws
            Throwable
    {
        if ( isClosed() )
            return;
        if ( isConstant() )
            return;

        JavaUtil.force( this::close );

        // Chain finalisers.
        super.finalize();
    }

    /**
     * Convert this object into the Java type system.  For input ports returns a
     * reader, for output ports a writer.
     *
     * @return The corresponding Java type for this object.
     */
    @Override
    public final Object toJava()
    {
        return _file;
    }

    /**
     * Return a port's string representation.
     *
     * @return A string representation for this port.
     */
    @Override
    final public String toString()
    {
        return String.format(
                "<%s '%s' %s %s>",
                getTypename( this ),
                name(),
                isBinary() ? "binary" : "textual",
                isClosed() ? "closed" : "open" );
    }
}
