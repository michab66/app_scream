/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.fcos;

import java.util.Objects;

import de.michab.scream.RuntimeX;
import de.michab.scream.Scream.Cont;
import de.michab.scream.pops.Primitives;
import de.michab.scream.util.Continuation.Thunk;

/**
 * The base class for all Scheme first-class objects.  A first class object is
 * an object that can be bound to a symbol.
 * <p>The static versions of the methods on this object offer additional
 * NIL handling over the non-static ones.
 * <p>Implementations of FirstClassObjects represent types in Scheme.  Each
 * implementation has to provide a string attribute named TYPE_NAME as a static
 * final member for full integration with type conversion messages.
 */
public abstract class FirstClassObject
{
    /**
     * Marks an fco as constant.  Constant objects cannot be modified.
     * Non-modifieable objects are by default constant.
     */
    private boolean _isConstant;

    /**
     * Evaluates the passed scheme object and returns the result.  Handles a NIL
     * object to evaluate.
     *
     * @param fco The FirstClassObject to evaluate.
     * @param env The environment for the evaluation.
     * @param c The target continuation.
     * @return A thunk.
     * @throws RuntimeX In case the evaluation failed.
     */
    public static Thunk evaluate( FirstClassObject fco, Environment env, Cont<FirstClassObject> c )
            throws RuntimeX
    {
        if ( fco == Cons.NIL )
            return c.accept( fco );

        return fco.evaluate( env, c );
    }

    /**
     * Evaluate this scheme object and return the result.  This default version
     * just evaluates to itself.  This must only be called if it is ensured that
     * the target object is not null.  If in doubt call FCO.evaluate( FCO, FCO ).
     *
     * @param e The environment used to evaluate the object.
     * @param c The continuation that receives the evaluation result
     * @return The thunk.
     * @throws RuntimeX In case the evaluation failed.
     */
    public Thunk evaluate(
        Environment e,
        Cont<FirstClassObject> c )
            throws RuntimeX
    {
        return  () -> {
            if ( _compiled == null )
                _compiled = _compile( e );
            return _compiled.evaluate( e, c );
        };
    }

    /**
     * The implementation of the Scheme {@code eqv?} procedure.  This static
     * method also handles NIL references.
     *
     * @param left The left operand for the comparison.
     * @param right The right operand of the comparison.
     * @return The result of the comparison.
     */
    public static boolean eqv( FirstClassObject left, FirstClassObject right )
    {
        // If one of the references is NIL...
        if ( Cons.NIL == left || Cons.NIL == right )
            // ...do a reference compare.  This is only successful if both object
            // references are NIL.
            return left == right;
        else
            return left.eqv( right );
    }

    /**
     * The implementation of the Scheme {@code eqv?} procedure.  The default
     * implementation delegates the operation to the {@code eq()} method.
     *
     * @param other The object to compare with.
     * @return The result of the comparison.
     */
    protected boolean eqv( FirstClassObject other )
    {
        return eq( other );
    }

    /**
     * The implementation of the scheme {@code eq?} procedure.  Handles NIL
     * references.
     *
     * @param left The left operand for the comparison.
     * @param right The right operand of the comparison.
     * @return The result of the comparison.
     */
    public static boolean eq( FirstClassObject left, FirstClassObject right )
    {
        // If one of the references is NIL...
        if ( Cons.NIL == left || Cons.NIL == right )
            // ...do a reference compare.  This is only successful if both object
            // references are NIL.
            return left == right;
        else
            return left.eq( right );
    }

    /**
     * The implementation of the scheme eq? procedure.  The default compares the
     * references.
     *
     * @param other The object to compare with.
     * @return The result of the comparison.
     */
    protected boolean eq( FirstClassObject other )
    {
        return this == other;
    }

    /**
     * The implementation of the scheme equal? procedure.
     *
     * @param left The left operand for the comparison.
     * @param right The right operand of the comparison.
     * @return The result of the comparison.
     */
    public static boolean equal( FirstClassObject left, FirstClassObject right )
    {
        // If one of the references is NIL...
        if ( Cons.NIL == left || Cons.NIL == right )
            // ...do a reference compare.  This is only successful if both object
            // references are NIL.
            return left == right;
        else
            return left.equal( right );
    }

    /**
     * The implementation of the scheme equal? procedure.  The default is to
     * delegate the comparison to the eqv() method.
     *
     * @param other The object to compare with.
     * @return The result of the comparison.
     */
    protected boolean equal( FirstClassObject other )
    {
        return eqv( other );
    }

    /**
     * Create a string from the passed object suitable for the read() operation.
     * To be able to also convert NILs to a string representation this is
     * implemented as a static method.
     *
     * @param object The object to be transformed to its string representation.
     * @return The string representation of the object.
     * @see java.lang.Object#toString
     */
    static public String toString( FirstClassObject object )
    {
        if ( Cons.NIL == object )
            return "()";
        else
            return object.toString();
    }

    /**
     * Create a string from this object suitable for the read() operation.
     *
     * @return The object's string representation.
     */
    @Override
    public abstract String toString();

    /**
     * Clone this scheme object.  The default implementation just returns itself.
     * This {@code static} version of this method additionally handles NIL
     * references.
     *
     * @param fco The first class object to be cloned.
     * @return A clone for the passed object.  Note that it depends on the type
     *         of the object whether this is a real clone or identical to the
     *         object passed in.
     */
    public static FirstClassObject copy( FirstClassObject fco )
    {
        if ( fco == Cons.NIL )
            return Cons.NIL;
        else
            return fco.copy();
    }

    /**
     * Copy this scheme object.  The default implementation returns identity which
     * can be used if the object cannot be modified.  Otherwise this need to be
     * overridden.
     *
     * @return A copy of the object.
     */
    protected FirstClassObject copy()
    {
        return this;
    }

    /**
     * Convert this {@code FirstClassObject} into its corresponding raw
     * Java representation.  If the object represents a
     * {@code SchemeDouble} this method would return an instance of
     * {@code java.lang.Double}.
     *
     * @return The corresponding object from the Java type system.
     * @throws RuntimeX
     */
    public abstract Object toJava() throws RuntimeX;

    /**
     * Get the symbolic typename.  Handles NIL references.
     *
     * @param o The object for which the typename gets computed.
     * @return The instances type name.
     * @see FirstClassObject#typename()
     */
    public static String getTypename( FirstClassObject o )
    {
        if ( o == Cons.NIL )
            return "NIL";

        return o.typename();
    }

    /**
     * Return the typename of this {@code FirstClassObject} instance.  This
     * is one of:<br>
     * {@code
     *  list
     *  symbol
     *  vector
     *  integer
     *  real
     *  string
     *  char
     *  object
     * }
     * The default implementation returns the value of the public final static
     * TYPE_NAME field. If this does not exist or is not accessible the name
     * of the class is returned.
     *
     * @return The instances type name.
     */
    public String typename()
    {
        return typename( getClass() );
    }
    public static String typename( Class<?> c )
    {
        String result = null;

        try
        {
            result = (String)c.getField( "TYPE_NAME" ).get( null );
        }
        catch ( Throwable t )
        {
            result = c.getName();
        }

        return result;
    }

    /**
     * Mark a {@code FirstClassObject} as constant.  Note that it is not
     * possible to switch from constant to variable, only the state change from
     * variable to constant is allowed.  Handles NIL values.
     *
     * @param fco The {@code FirstClassObject} to modify.
     * @param what The new value.
     * @throws IllegalArgumentException In case it has been tried to change the
     *         state of the {@code FirstClassObject} from constant to
     *         variable.
     */
    static void setConstant( FirstClassObject fco, boolean what )
    {
        if ( Cons.NIL != fco )
            fco.setConstant( what );
    }

    /**
     * Mark a {@code FirstClassObject} as constant. Handles NIL values.
     *
     * @param fco The {@code FirstClassObject} to modify.
     */
    public static <T extends FirstClassObject>
    T setConstant( T fco )
    {
        if ( Cons.NIL != fco )
            fco.setConstant( true );
        return fco;
    }

    /**
     * Can be used to mark a {@code FirstClassObject} as constant.  Note
     * that it is not possible to switch from constant to variable, only the
     * state change from variable to constant is allowed.
     *
     * @param what The new value.
     * @throws IllegalArgumentException In case it has been tried to change the
     *         state of the {@code FirstClassObject} from constant to
     *         variable.
     */
    void setConstant( boolean what )
    {
        if ( _isConstant && !what )
            throw new IllegalArgumentException(
                    "Constant->variable change not allowed." );

        _isConstant = what;
    }

    /**
     * Returns whether an {@code FirstClassObject} is a constant. Constant
     * objects are the self-evaluating primitives and can be created by the
     * {@code quote} syntax.
     *
     * @param fco The {@code FirstClassObject} to test.
     * @return Whether this should be seen as a constant.
     */
    public static boolean isConstant( FirstClassObject fco )
    {
        return fco == Cons.NIL || fco.isConstant();
    }

    /**
     * Returns whether an {@code FirstClassObject} is a constant. Constant
     * objects are the self-evaluating primitives and can be created by the
     * {@code quote} syntax.
     *
     * @return Whether this should be seen as a constant.
     */
    public boolean isConstant()
    {
        return _isConstant;
    }

    private static Lambda _NIL = new Lambda(
            (e,c) -> Primitives._x_quote(
                    e,
                    Cons.NIL,
                    c ),
            "NIL" );

    public static Lambda _compile( FirstClassObject fco, Environment env )
            throws RuntimeX
    {
        if ( fco == Cons.NIL )
            return _NIL;
        return fco._compile( env );
    }

    protected Lambda _compile( Environment env )
        throws RuntimeX
    {
        return new Lambda(
                (e,c) -> Primitives._x_quote(
                        e,
                        this,
                        c ),
                this.toString() );
    }

    private Lambda _compiled;

    private static int _s_id = 0;

    /**
     * The fco's unique id. Used in debugging.
     */
    private final int _id = ++_s_id;

    /**
     * @return Get the unique id of this fco for debugging.
     */
    protected final int id()
    {
        return _id;
    }

    /**
     * Check if the passed fco is of a certain type.
     * <p>
     * A null object results in true for the Cons.class.
     *
     * @param <T> Class type, must extend {@link FirstClassObject}.
     * @param type The target type.
     * @param fco The object to test.
     * @return true if the object is of the requested type.
     */
    public static <T extends FirstClassObject>
    boolean is( Class<T> type, FirstClassObject fco )
    {
        Objects.requireNonNull( type );

        if ( type.equals( Cons.class ) && fco == Cons.NIL )
            return true;

        if ( fco == null )
            return false;

        return type.isAssignableFrom( fco.getClass() );
    }
}
