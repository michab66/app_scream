/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import org.smack.util.collections.WeakMapWithProducer;

import de.michab.scream.pops.Primitives;

/**
 * Represents the Scheme symbol type.
 */
public final class Symbol
extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "symbol";

    /**
     * This symbol's name.
     */
    private final String _name;

    /**
     *
     */
    private static WeakMapWithProducer<String, Symbol> _flyweightMap =
            new WeakMapWithProducer<>( c -> new Symbol( c ) );

    /**
     * The symbol factory.  This has to be used to create new symbols.
     *
     * @param name The new symbol's name.
     * @return The newly created symbol.
     */
    public static synchronized Symbol createObject( String name )
    {
        return _flyweightMap.get( name );
    }

    /**
     * The private constructor.  To create new symbols the static factory method
     * createObject has to be used.
     *
     * @param name The new symbol's name.
     * @see #createObject
     */
    private Symbol( String name )
    {
        _name = name;
        setConstant();
    }

    @Override
    protected Lambda _compile()
    {
        return new Lambda(
                (e,c) ->
                    Primitives._resolve( e, this, c ),
                "resolve " + toString() );
    }

    /**
     * Tests equivalence to another object.
     *
     * @param other The other object used for the comparison.
     * @return {@code true} if the objects were equal.
     * @see FirstClassObject#eq
     */
    @Override
    public boolean eq( FirstClassObject other )
    {
        return equals( other );
    }

    /**
     * Transform this symbol to its string representation.
     *
     * @return A string representation for this object.
     * @see FirstClassObject#toString
     */
    @Override
    public String toString()
    {
        return _name;
    }

    /**
     * Convert this object into the Java type system.  Symbols are converted into
     * a string equivalent.
     *
     * @return This symbol as a string.
     */
    @Override
    public String toJava()
    {
        return toString();
    }

    /**
     * The implementation of the standard {@code java.lang.Object.hashCode()}
     * method.
     *
     * @return An hashcode for the object instance.
     */
    @Override
    public int hashCode()
    {
        return _name.hashCode();
    }

    /**
     *
     */
    @Override
    public boolean equals( Object other )
    {
        if ( other == Cons.NIL )
            return false;

        try
        {
            return _name.equals( ((Symbol)other)._name );
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }
}
