/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.smack.util.CachedHolder;
import org.smack.util.JavaUtil;
import org.smack.util.StringUtil;

import de.michab.scream.RuntimeX;

/**
 * An Environment is an associative array indexed by Symbols.  Each environment
 * holds a set of symbol-value relations called bindings.  Environments can be
 * extended with sub environments.
 *
 * Symbol resolution checks first in the current environment for the symbol and
 * after that recursively on the parent environments up to the
 * top-level-environment which has no parent.
 *
 * @author Michael G. Binz
 */
public final class Environment
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "Environment";

    /**
     * This environment's name.  Used for tracking of the creator of the
     * environment in error reports.
     */
    private final Symbol _name;

    /**
     * A reference to the parent environment.
     */
    private final Environment _parent;

    /**
     * The hash table holding the environment's entries.
     */
    private final HashMap<Symbol, FirstClassObject> _symbolMap =
            new HashMap<>();

    /**
     * Create an empty environment without a parent.  Useful as a starting
     * point for creating a top level environment, i.e. an environment that holds
     * the system-defined bindings.
     *
     * @param name A name for the environment.
     * @return An environment.
     */
    public Environment( String name )
    {
        this( Symbol.createObject( name ), null );
    }

    /**
     * Construct an Environment with the given parent environment.
     *
     * @param name A symbolic name tied to the environment.  Used for marking an
     *             Environment instance with the name of the procedure that
     *             created the environment.
     * @param parent A reference to a parent environment.
     */
    private Environment( Symbol name, Environment parent )
    {
        JavaUtil.Assert(
                StringUtil.hasContent( name.toString() ) );
        _name = Objects.requireNonNull(
                name );
        _parent =
                parent;
    }

    /**
     * Create a nested Environment with a symbolic name.
     *
     * @param name The name of the new Environment.
     * @return A nested environment.
     */
    public Environment extend( Symbol name )
    {
        return new Environment( name, this );
    }
    public Environment extend( String name )
    {
        return extend( Symbol.createObject( name ) );
    }

    /**
     * @return The number of symbols defined in the environment.
     */
    public long size()
    {
        return _symbolMap.size();
    }

    /**
     * @return The environment's name.
     */
    public Symbol getName()
    {
        return _name;
    }

    /**
     * Create a new entry in this environment.  If the entry already exists,
     * it will be replaced.
     *
     * @param symbol The symbol to bind.
     * @param value The value to bind.
     * @return The environment.
     * @throws RuntimeX If the environment is constant.
     * @see #assign(Symbol, FirstClassObject)
     */
    public Environment define( Symbol symbol, FirstClassObject value ) throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        _symbolMap.put( symbol, value );
        return this;
    }

    /**
     * Remove a symbol from this environment.  This operation is normally neither
     * available nor needed in Scheme but is of help after the init phase to be
     * able to remove temporary bound symbols.
     *
     * @param symbol The symbol to undefine.
     * @throws RuntimeX
     */
    public synchronized void unset( Symbol symbol )
            throws RuntimeX
    {
        if ( _symbolMap.containsKey( symbol ) )
        {
            if ( isConstant() )
                throw RuntimeX.mCannotModifyConstant( symbol );

            _symbolMap.remove( symbol );
        }

        if ( _parent != null )
            _parent.unset( symbol );
    }

    /**
     * Create an array of all symbols defined in this environment and its
     * parent environments.
     *
     * @return An array of all symbols defined in this environment.
     */
    public Symbol[] getDefinedSymbols()
    {
        Map<Symbol, FirstClassObject> allSymbols = getDefinedSymbolsImpl();

        Symbol[] symbolArray =
                allSymbols.keySet().toArray( new Symbol[ allSymbols.size() ] );

        return symbolArray;
    }

    /**
     * Collect all symbols defined in the environment and its parent
     * environments.
     *
     * @return A hashtable including all symbols defined in this environment.
     */
    private Map<Symbol, FirstClassObject> getDefinedSymbolsImpl()
    {
        Map<Symbol, FirstClassObject> result;

        if ( _parent == null )
            result = new HashMap<Symbol, FirstClassObject>();
        else
            result = _parent.getDefinedSymbolsImpl();

        result.putAll( _symbolMap );

        return result;
    }

    /**
     * Assign a new value to an existing entry in this environment.  The symbol
     * has to exist in the environment or one of the parent environments.
     *
     * @param symbol The symbol to assign to.
     * @param value The value to assign.
     * @see #define(Symbol, FirstClassObject)
     * @throws RuntimeX If the symbol to be assigned didn't exist in this
     *         environment.
     */
    public Environment assign( Symbol symbol, FirstClassObject value )
            throws RuntimeX
    {
        if ( _symbolMap.containsKey( symbol ) )
        {
            if ( isConstant() )
                throw RuntimeX.mCannotModifyConstant( this );

            return define( symbol, value );
        }

        if ( _parent != null )
            return _parent.assign( symbol, value  );

        throw RuntimeX.mSymbolNotAssignable( symbol );
    }

    /**
     * Access an environment entry.
     *
     * @param symbol The symbol to dereference.
     * @return The value bound to the passed symbol.
     * @throws RuntimeX If the symbol didn't exist.
     */
    public FirstClassObject get( Symbol symbol )
            throws RuntimeX
    {
        if ( _symbolMap.containsKey( symbol ) )
            return _symbolMap.get( symbol );
        if ( _parent != null )
            return _parent.get( symbol );

        throw RuntimeX.mSymbolNotDefined( symbol );
    }

    /**
     * Set a primitive operation.  Primitive in this context means an operation
     * that is implemented as part of the Scream runtime system in Java.  A
     * primitive operation must have a name that is not the default name that
     * is defined for unnamed operations.
     *
     * @param op A reference to the primitive operation to set.
     * @throws RuntimeX
     * @see Operation#DEFAULT_NAME
     * @exception IllegalArgumentException Is thrown when the operation's name is
     *            the default name.
     */
    public void setPrimitive( Operation op )
            throws RuntimeX
    {
        Symbol name = op.getName();

        if ( name == Operation.DEFAULT_NAME )
            throw new IllegalArgumentException( "Primitives need to be named." );

        define( name, op );
    }

    @Override
    public HashMap<Symbol, FirstClassObject> toJava()
    {
        return _symbolMap;
    }

    private String parentNames()
    {
        if ( _parent == null )
        {
            return String.format( "/%s(%d)",
                    _name.toString(),
                    id() );
        }
        else
        {
            return  String.format( "%s/%s(%d)",
                _parent._parentNames.get(),
                _name.toString(),
                id() );
        }
    }

    private CachedHolder<String> _parentNames =
            new CachedHolder<String>( this::parentNames );

    @Override
    public String toString()
    {
        StringBuilder name =
                new StringBuilder( "<Environment:" );
        name.append( _parentNames.get() );
        name.append( ">" );

        return name.toString();
    }
}
