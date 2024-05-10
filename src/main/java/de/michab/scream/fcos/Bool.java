/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.fcos;

/**
 * Represents the Scheme boolean type.
 *
 * @author Michael Binz
 */
public class Bool
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "boolean";

    /**
     * The single true value.
     */
    public static final Bool T = new Bool();

    /**
     * The single false value.
     */
    public static final Bool F = new Bool();

    /**
     * A factory for scheme booleans.
     */
    static public Bool createObject( boolean v )
    {
        return v ? T : F;
    }

    /**
     * Constructor is private.  Access only allowed via the static members T/F.
     */
    private Bool()
    {
        setConstant();
    }

    /**
     * @param fco The object to test.
     * @return true for all objects different from #f.
     */
    public static boolean isTrue( FirstClassObject fco )
    {
        return fco != F;
    }

    /**
     * Access the boolean's value as a primitive java type.
     *
     * @return The primitive value.
     * @see Bool#convertToJava() for the wrapped value.
     */
    public boolean getValue()
    {
        return this == T;
    }

    /**
     * @return A string representation of this object.
     */
    @Override
    public String toString()
    {
        return this == T ? "#T" : "#F";
    }

    /**
     * Convert this instance into the native Java type system.  Booleans are
     * represented by {@code Boolean.TRUE} and {@code Boolean.FALSE}.
     *
     * @return {@code Boolean.TRUE} or {@code Boolean.FALSE} depending
     *         on the object's value.
     * @see Bool#getValue() for access to the primitive value.
     */
    @Override
    public Boolean toJava()
    {
        return this == T ? Boolean.TRUE : Boolean.FALSE;
    }
}
