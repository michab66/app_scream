/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.io.Closeable;
import java.io.IOException;
import java.lang.ref.Cleaner;
import java.util.logging.Logger;

import org.smack.util.JavaUtil;

import de.michab.scream.RuntimeX;
import de.michab.scream.frontend.SchemeParser;

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
    private static Logger LOG = Logger.getLogger( Port.class.getName() );

    // Garbage collection: We want to close a port even in the case
    // that a Scheme program forgets the reference to the port object.

    /**
     * Garbage collection: The cleaner needed.  Another cleaner
     * could also be used. (Integrate one in Smack?)
     */
    private static final Cleaner _cleaner = Cleaner.create();

    /**
     * Garbage collection: A local type that holds the state
     * information needed to close the stream.
     * @param <C>
     */
    static class State<C extends Closeable> implements Runnable
    {
        public String _name;
        public C _stream =
                null;
        public boolean _isConstant =
                false;

        @Override
        public void run()
        {
            // Note that we get calls here for *all* port instances,
            // even the closed ones.
            if ( _stream == null )
                return;
            if ( _isConstant )
                return;
            LOG.warning( "closing lost port: " + _name );
            JavaUtil.force( _stream::close );
        }
    }

    /**
     * Garbage collection: An instance of our state.
     * This must not refer to the enclosing port object.
     */
    private final State<T> _state =
            new State<>();

    /**
     * Garbage collection: Per-instance default initialization.  Registers
     * our port instance and the respective state.  If the instance
     * is garbage collected, the state's run operation is called.
     */
    {
        _cleaner.register( this, _state );
    }

    public static final String TYPE_NAME = "port";

    /**
     * An object representing EOF.
     */
    public static final FirstClassObject EOF = makeEofObject();

    private static FirstClassObject makeEofObject()
    {
        try
        {
            return FirstClassObject.setConstant(
                    new SchemeParser( "#(EOF)" ).getExpression() );
        }
        catch (Exception e) {
            throw new InternalError( e );
        }
    }


    /**
     * @return The port's name, commonly a filename.
     */
    private final String _name;

    /**
     * @return The port's stream.
     */
    public T stream()
    {
        return _state._stream;
    }

    /**
     * @param file Set the port's stream.
     */
    protected void stream( T file )
    {
        _state._stream = file;
    }

    /**
     * Create a port.
     *
     * @param name The port's name.
     */
    protected Port( String name )
    {
        _name = name;
        _state._name = toString() + "id=" + id();
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
        return stream() == null;
    }

    public abstract SchemeBoolean isBinary();

    /**
     * Closes the port.
     */
    // Garbage collection: This is deliberately *not* the
    // same implementation like the State#close operation
    // since for this close operation we want a proper
    // exception handling.
    @Override
    public final void close() throws RuntimeX
    {
        if ( isClosed() )
            return;
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        try
        {
            // Garbage collection: first get the state, keep it locally ...
            var hold = stream();
            // ... and set the state to null.
            stream( null );
            // Then finally close the state. If this
            // results in an exception, this is
            // properly handled, but we switched now
            // to closed.
            hold.close();
        }
        catch ( IOException e )
        {
            throw RuntimeX.mIoError( e );
        }
    }

    /**
     * Convert this object into the Java type system.  The concrete
     * type returned depends on the Port-implementation.
     *
     * @return The corresponding Java type for this object.
     * {@code null} is returned if the port is closed.
     */
    @Override
    public final T toJava()
    {
        return stream();
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
                "#<%s '%s' %s %s>",
                getTypename( this ),
                name(),
                isBinary() == SchemeBoolean.T ? "binary" : "textual",
                isClosed() ? "closed" : "open" );
    }

    @Override
    // Garbage collection:  We need to track the constantness
    // to prevent a close of non-closable ports.
    public final FirstClassObject setConstant()
    {
        _state._isConstant = true;

        return super.setConstant();
    }
}
