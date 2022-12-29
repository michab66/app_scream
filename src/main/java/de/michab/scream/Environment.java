/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import java.util.HashMap;
import java.util.Map;

import de.michab.scream.ScreamException.Code;

/**
 * An Environment is an associative array indexed by Symbols.  Each environment
 * holds a set of symbol-value relations called bindings.  Environments can be
 * extended with sub environments.
 *
 * Symbol resolution checks first in the environment for the symbol and after
 * that recursively on the parent environments up to the top-level-environment
 * or TLE which is without a parent.
 *
 * Environments can be named, i.e. on extension time a symbol naming the new
 * environment can be given.  If no name is given, e.g. by calling the version
 * of <code>extend()</code> without parameters then the name of the parent
 * environment is inherited.  The top-level-environment has a symbolic name of
 * 'top-level'.  Naming environments is used in Scream to place the name of the
 * procedure that created the environment for error reporting.
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
     * A reference to our parent environment.
     *
     * @label parent
     */
    private final Environment _parent;

    /**
     * The hash table to which we delegate our map responsibilities.
     */
    private final HashMap<Symbol, FirstClassObject> _symbolMap =
            new HashMap<>();

    /**
     * Create an empty environment without a parent.  Useful as a starting
     * point for creating a top level environment, i.e. an environment that holds
     * the system-defined bindings.<br>
     * The created environment has the name 'top-level'.
     */
    public Environment()
    {
        this( Symbol.createObject( "top-level" ), null );
    }

    /**
     * Copy constructor.  Used to clone the top level environment.
     *
     * @param p The Environment to copy.
     */
    public Environment( Environment p )
    {
        _name =
                p._name;
        _parent =
                p._parent;
        _symbolMap.putAll(
                p._symbolMap );
    }

    /**
     * Construct a new Environment with the given parent environment.
     *
     * @param name A symbolic name tied to the environment.  Used for marking an
     *             Environment instance with the name of the procedure that
     *             created the environment.
     * @param parent A reference to a parent environment.
     */
    private Environment( Symbol name, Environment parent )
    {
        // Note that this will fail if a null parent is passed and the name is
        // also null.  But we are in private context here, the default ctor takes
        // care for initialising the base environment's name.
        if ( name == null )
            _name = parent.getName();
        else
            _name = name;

        _parent = parent;
    }

    /**
     * Create a nested Environment with a symbolic name.
     *
     * @param name The name for the new Environment.
     * @return A nested environment.
     */
    public Environment extend( Symbol name )
    {
        return new Environment( name, this );
    }

    /**
     * Return this environment's symbolic name.  The name is inherited, i.e. if
     * no name was given the parent environment's name will be returned.
     *
     * @return The environment's symbolic name.
     * @see Environment#extend( Symbol name )
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
     * @see #assign(Symbol, FirstClassObject)
     */
    public Environment define( Symbol symbol, FirstClassObject value )
    {
        _symbolMap.put( symbol, value );
        return this;
    }

    /**
     * Remove a symbol from this environment.  This operation is normally neither
     * available nor needed in Scheme but is of help after the init phase to be
     * able to remove temporary bound symbols.
     *
     * @param symbol The symbol to undefine.
     */
    public synchronized void unset( Symbol symbol )
    {
        if ( _symbolMap.containsKey( symbol ) )
            _symbolMap.remove( symbol );
        else if ( _parent != null )
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
            return define( symbol, value );
        else if ( _parent != null )
            return _parent.assign( symbol, value  );
        else
            throw new RuntimeX( Code.SYMBOL_NOT_ASSIGNABLE, symbol );
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
        else if ( _parent != null )
            return _parent.get( symbol );
        else
            throw new RuntimeX( Code.SYMBOL_NOT_DEFINED, symbol );
    }

    /**
     * Set a primitive operation.  Primitive in this context means an operation
     * that is implemented as part of the Scream runtime system in Java.  A
     * primitive operation must have a name that is not the default name that
     * is defined for unnamed operations.
     *
     * @param op A reference to the primitive operation to set.
     * @see Operation#DEFAULT_NAME
     * @exception IllegalArgumentException Is thrown when the operation's name is
     *            the default name.
     */
    public void setPrimitive( Operation op )
    {
        Symbol name = op.getName();

        if ( name == Operation.DEFAULT_NAME )
            throw new IllegalArgumentException( "Primitives need to be named." );

        define( name, op );
    }

//    /**
//     * (evaluate <expression>)
//     *
//     * Currently the environment arguments are not supported.
//     */
//    static private Procedure evaluateProcedure = new Procedure( "evaluate" )
//    {
//        private Class<?>[] formalArglist = new Class[]{ FirstClassObject.class };
//
//        @Override
//        public FirstClassObject apply( Environment parent, FirstClassObject[] args )
//                throws RuntimeX
//        {
//            checkArguments( formalArglist, args );
//
//            return evaluate( args[0], parent );
//        }
//    };
//
    /**
     * Environment operations setup.
     *
     * @param tle A reference to the system private top level environment.
     * @return A reference to the environment including the additional entries
     *        defined by this class.
     */
    public static Environment extendTopLevelEnvironment( Environment tle )
    {
        //tle.setPrimitive( evaluateProcedure );
        return tle;
    }

    @Override
    public Object toJava()
    {
        return _symbolMap;
    }

    @Override
    public String toString()
    {
        StringBuilder name =
                new StringBuilder( "<Environment " );

        if ( _name == null )
            name.append( "anonymous" );
        else
            name.append( _name );

        name.append( ">" );

        return name.toString();
    }
}
