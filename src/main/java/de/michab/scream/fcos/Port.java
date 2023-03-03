/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

/**
 * Represents a scheme port object.  Ports represent input and output devices.
 * To Scheme, an input port is a Scheme object that can deliver characters
 * upon command, while an output port is a Scheme object that can accept
 * characters.
 * <p>
 * See 'r7rs, 6.13 Input and output' for details.
 *
 * @author Michael Binz
 */
public abstract class Port
    extends
        FirstClassObject
    implements
        AutoCloseable
{
    protected enum Type{ Binary, Textual };
    public static final String TYPE_NAME = "port";

    /**
     * An object representing EOF.
     */
    public static final Symbol EOF = Symbol.createObject( "EOF" );

    private final String _name;

    private final Type _type;

    /**
     * Create a textual port.
     *
     * @param name The port's name.
     */
    public Port( String name )
    {
        this( name, Type.Textual );
    }
    /**
     * Create a typed port.
     *
     * @param name The port's name.
     * @param type The port's type.
     */
    public Port( String name, Type type )
    {
        _name = name;
        _type = type;
    }

    String name()
    {
        return _name;
    }

    public Type type()
    {
        return _type;
    }

    /**
     * @return {@code true} if this is a binary port.
     */
    // Called from Scheme.  TODO check if we can call type().
    public boolean isBinary()
    {
        return _type == Type.Binary;
    }

    /**
     * Tests if the port is closed.
     *
     * @return {@code true} if the port is closed, {@code false} if the
     * port is open.
     */
    public abstract boolean isClosed();

    /**
     * Closes the port.  This is also done from the finalizer.
     */
    @Override
    public abstract void close();

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
        close();
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
    public abstract Object toJava();

    /**
     * Extends the passed environment with the procedures logically associated
     * with this class.
     *
     * @param tle The environment to extend.
     * @return The passed environment after extension.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        tle.define( EOF, EOF );

        return tle;
    }
}
