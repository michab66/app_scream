/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import java.util.HashSet;

import org.smack.util.Holder;

import de.michab.scream.Continuation.Cont;
import de.michab.scream.Continuation.Thunk;
import de.michab.scream.Lambda.L;
import de.michab.scream.util.Scut;

/**
 * Represents a list cell.  A list cell consists of two references called car
 * and cdr.  Car is a reference to the value of the current node of the list
 * while cdr is a reference to the next list cell.  Lists are made up of cons
 * cells where each car points to an object and the cdrs are pointing to the
 * cons in the list.
 * <p>
 * A list is called proper when the last cdr is NIL.
 *
 * @author Michael G. Binz
 */
public class Cons
    extends FirstClassObject
{
    /**
     * The name of the type as used by error reporting.
     *
     * @see FirstClassObject#typename()
     */
    public static final String TYPE_NAME = "cons";

    /**
     * A constant representing the empty list.
     */
    public static final Cons NIL = null;

    /**
     * A reference to this node's value.
     */
    private FirstClassObject _car;

    /**
     * A reference to the next node in this list.
     */
    private FirstClassObject _cdr;

    enum Comparison { Eq, Eqv, Equal };

    /**
     * Construct a {@code Cons} cell from a given car and cdr element.
     *
     * @param car The value for the new cons cell.
     * @param cdr The next cons cell.
     */
    public Cons( FirstClassObject car, FirstClassObject cdr )
    {
        _car = car;
        _cdr = cdr;
    }

    /**
     * Creates a newly allocated list from a Java array.  This is not available
     * to the public because we can't handle empty arrays within this
     * constructor.  So to create a {@code Cons} from an array the static
     * {@code create()} method has to be used.
     *
     * @param a The array to transform.  In case the array is empty an
     *          IllegalArgumentException is thrown.
     * @param start The start index of the array to transform.
     * @throws IllegalArgumentException In case parameter {@code a} is an
     *         empty array.
     * @see de.michab.scream.Cons#create(FirstClassObject[])
     */
    private Cons( FirstClassObject[] a, int start )
    {
        if ( a.length == 0 )
            throw new IllegalArgumentException( "Can't create Cons for empty array" );

        // Initialize our cdr...
        _cdr = NIL;
        // ...and append all array elements but the first one in reverse order.
        for ( int i = a.length -1 ; i > start ; i-- )
            _cdr = new Cons( a[i], _cdr );
        // Put the first one into our car.
        _car = a[ start ];
    }

    /**
     * Transforms an array of FirstClassObjects into a list.  In case the array
     * is empty the empty list NIL is returned.
     *
     * @param a The array to be transformed into a Cons list.
     * @param start The start index into the array.
     * @return A newly allocated cons cell.
     */
    public static Cons create( FirstClassObject[] a, int start )
    {
        Cons result = NIL;

        if ( a.length > 0 )
            result = new Cons( a, start );

        return result;
    }

    /**
     * Transforms an array of FirstClassObjects into a list.  In case the array
     * is empty the empty list NIL is returned.
     *
     * @param a The array to be transformed into a Cons list.
     * @return A newly allocated cons cell.
     */
    public static Cons create( FirstClassObject ... a )
    {
        return create( a, 0 );
    }

    /**
     * Get this cons' value.
     *
     * @return This cons cell value.
     */
    public FirstClassObject getCar()
    {
        return _car;
    }

    /**
     * Set this cons' value.
     *
     * @param car The new cons cell value to set.
     * @throws RuntimeX If it was tried to modify a constant list.
     */
    public void setCar( FirstClassObject car )
            throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        _car = car;
    }

    /**
     * Get this cons' successor.
     *
     * @return This cell's follow-up cell.  Could be NIL if this is the last cell
     * in the list.
     */
    public FirstClassObject getCdr()
    {
        return _cdr;
    }

    /**
     * Set this cons' successor.
     *
     * @param cdr The new successor to set.
     * @throws RuntimeX If it was tried to modify a constant list.
     */
    public void setCdr( FirstClassObject cdr )
            throws RuntimeX
    {
        if ( isConstant() )
            throw RuntimeX.mCannotModifyConstant( this );

        _cdr = cdr;
    }

    /**
     * Returns a new copy of this list with tail appended.  Note that tail isn't
     * copied (see r5rs 27).
     *
     * @param tail The list to append.
     * @return The concatenated list.
     * @throws RuntimeX in case this list is not proper.
     */
    public Cons append( FirstClassObject tail )
            throws RuntimeX
    {
        if ( ! isProperList() )
            throw RuntimeX.mExpectedProperList();

        // Copy the list (but not the contents).
        Cons thisCopy = Cons.create( asArray() );
        // Create a work pointer.
        Cons thisPointer = thisCopy;
        // Seek to the end...
        while ( thisPointer._cdr != NIL )
            thisPointer = (Cons)thisPointer._cdr;
        // ...and append the tail.
        thisPointer._cdr = tail;

        return thisCopy;
    }

    /**
     * Returns a newly allocated list consisting of the elements of the input
     * list in reverse order.
     *
     * @return The reversed list.
     * @throws RuntimeX In case this list is not proper.
     */
    public Cons reverse()
            throws RuntimeX
    {
        if ( ! isProperList() )
            throw RuntimeX.mExpectedProperList();

        FirstClassObject[] result = asArray();

        for ( int mid = result.length / 2,
                i = 0 ; i < mid ; i++ )
        {
            FirstClassObject tmp = result[i];
            result[i] = result[ result.length-1 - i ];
            result[ result.length-1 - i ] = tmp;
        }

        return Cons.create( result );
    }
    public static Cons reverse( Cons c )
            throws RuntimeX
    {
        if ( c == Cons.NIL )
            return c;

        return c.reverse();
    }

    /**
     * Returns the sublist of list obtained by omitting the first k elements. It
     * is an error if list has fewer than k elements.
     *
     * @param k The index of the first element of the sublist to return.
     * @return The sublist.
     * @throws RuntimeX In case k denotes no valid list element index.
     */
    public Cons listTail( long k )
            throws RuntimeX
    {
        if ( k > length() || k < 0 )
            throw RuntimeX.mIndexOutOfBounds( k );

        Cons result = this;

        for ( long i = 0 ; i < k ; i++ )
            result = Scut.as( Cons.class, result._cdr );

        return result;
    }

    /**
     * Returns the kth element of the list.  This is the same as
     * {@code listTail(x).getCar()}.  List indices are zero based.
     *
     * @param k The index of the element to return.  The index is zero based.
     * @return The kth element.
     * @throws RuntimeX In case k is no valid list element index.
     */
    public FirstClassObject listRef( long k )
            throws RuntimeX
    {
        var result = listTail( k );

        if ( result == Cons.NIL )
            throw RuntimeX.mIndexOutOfBounds( k );

        return result.getCar();
    }

    /**
     * The common implementation of the list member functionality.  Returns a
     * sublist starting with the passed element.
     *
     * @param obj The element to look for.
     * @param cmpFct Specifies the comparison procedure to use.
     * @return A sublist starting with the passed obj.
     * @throws RuntimeX In case an error occurs.
     */
    private FirstClassObject memx( FirstClassObject obj, Comparison cmpFct )
            throws RuntimeX
    {
        // TODO not very efficient:  ListTail does more than needed.
        long thisLength = length();

        for ( long i = 0 ; i < thisLength ; i++ )
        {
            Cons subList = listTail( i );
            FirstClassObject subListsCar = subList._car;

            if ( ( cmpFct == Comparison.Eq && eq( obj, subListsCar ) ) ||
                    ( cmpFct == Comparison.Eqv && eqv( obj, subListsCar ) ) ||
                    ( cmpFct == Comparison.Equal && equal( obj, subListsCar ) ) )
                return subList;
        }

        return SchemeBoolean.F;
    }

    /**
     * @param obj The object to find in the list.
     * @return The sublist starting with the passed element.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject memq( FirstClassObject obj )
            throws RuntimeX
    {
        return memx( obj, Comparison.Eq );
    }

    /**
     * @param obj The object to find in the list.
     * @return The sublist starting with the passed element.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject memv( FirstClassObject obj )
            throws RuntimeX
    {
        return memx( obj, Comparison.Eqv );
    }

    /**
     * @param obj The object to find in the list.
     * @return The sublist starting with the passed element.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject member( FirstClassObject obj )
            throws RuntimeX
    {
        return memx( obj, Comparison.Equal );
    }

    /**
     * The common implementation of the association list functionality.
     *
     * @param obj The object to look up in the association list.
     * @param cmpFct A selector for the comparison function to use.
     * @throws RuntimeX In case an error occurs.
     * @see #EQ
     * @see #EQV
     * @see #EQUAL
     */
    private FirstClassObject assx( FirstClassObject obj, Comparison cmpFct )
            throws RuntimeX
    {
        // TODO not very efficient:  ListRef does more than needed.
        long thisLength = length();

        for ( long i = 0 ; i < thisLength ; i++ )
        {
            FirstClassObject subList = listRef( i );

            if ( ! (subList instanceof Cons) )
                throw RuntimeX.mInvalidAssocList(  this );

            FirstClassObject subListsCar = ((Cons)subList)._car;

            if ( ( cmpFct == Comparison.Eq && eq( obj, subListsCar ) ) ||
                    ( cmpFct == Comparison.Eqv && eqv( obj, subListsCar ) ) ||
                    ( cmpFct == Comparison.Equal && equal( obj, subListsCar ) ) )
                return subList;
        }

        return SchemeBoolean.F;
    }

    /**
     * @param obj The object to find in the association list.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject assq( FirstClassObject obj )
            throws RuntimeX
    {
        return assx( obj, Comparison.Eq );
    }

    /**
     * @param obj The object to find in the association list.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject assv( FirstClassObject obj )
            throws RuntimeX
    {
        return assx( obj, Comparison.Eqv );
    }

    /**
     * @param obj The object to find in the association list.
     * @throws RuntimeX In case an error occurs.
     */
    public FirstClassObject assoc( FirstClassObject obj )
            throws RuntimeX
    {
        return assx( obj, Comparison.Equal );
    }

    private static Thunk performInvocation(
            Environment e,
            FirstClassObject  op,
            Cons args,
            Cont<FirstClassObject> c )
        throws RuntimeX
    {
        try
        {
            return ((Operation)op)._execute( e, args, c );
        }
        catch ( NullPointerException | ClassCastException x )
        {
            throw RuntimeX.mCalledNonProcedural( op ).addCause( x );
        }
    }

    @Override
    protected Lambda _compile( Environment env ) throws RuntimeX
    {
        if ( ! isProperList() )
            throw RuntimeX.mExpectedProperList();

        var car = getCar();
        var cdr = Scut.as( Cons.class, getCdr() );

        L l = (e,c) -> {
            return FirstClassObject.evaluate(
                    car,
                    e,
                    op -> performInvocation( e, op, cdr, c ) );
        };

        return new Lambda( l, this.toString()  );
    }

    /**
     * The implementation of the scheme equal? procedure.  This is the least
     * efficient one since lists and arrays are deep compared. For other types
     * eqv? is used.
     *
     * @param other The object to compare with.
     * @return {@code true} if the passed object is equal to this cons.
     */
    @Override
    public boolean equal( FirstClassObject other )
    {
        if ( other == NIL )
            return false;

        try
        {
            Cons otherCons = (Cons)other;

            return equal( _car, otherCons._car ) &&
                    equal( _cdr, otherCons._cdr );
        }
        catch ( ClassCastException e )
        {
            return false;
        }
    }

    /**
     * @return A string representation for this list.
     */
    @Override
    public String toString()
    {
        if ( isCircular() )
            return "#circular#";

        if ( _cdr instanceof Cons || _cdr == NIL )
        {
            // Stringise the cdr.  This will lead to something like "(n0 n1 ...)".
            String result = toString( _cdr );
            // We have to insert our stringised car just after the leading brace to
            // get proper list notation.  So first remove the leading brace...
            result = result.substring( 1 );
            // ..and glue a new brace and the stringised car to the beginning of the
            // result string.
            return "(" +
            toString( _car ) +
            ((! result.startsWith( ")" )) ? " " : "") +
            result;
        }

        return "(" + toString( _car ) + " . " + toString( _cdr ) + ")";
    }

    /**
     * @return A recursive copy of this list.
     */
    @Override
    public Cons copy()
    {
        return new Cons(
                copy( _car ),
                copy( _cdr ) );
    }

    /**
     * @param lengthOut The list length.  Valid in all cases.
     * @return An array of booleans, index 0 denotes circularity, index 1 is
     * true element if the list is proper.
     */
    private boolean[] consAttributes( Holder<Long> lengthOut )
    {
        HashSet<Cons> collector = new HashSet<Cons>();

        long length = 0;

        try
        {
            for ( Cons c = this ; c != Cons.NIL ; c = (Cons)c._cdr )
            {
                length++;
                if ( collector.contains( c ) )
                {
                    lengthOut.set( length );
                    return new boolean[]{ true, false };
                }

                collector.add( c );
            }
        }
        catch ( ClassCastException ignore )
        {
            lengthOut.set( length );
            return new boolean[]{ false, false };
        }

        lengthOut.set( length );
        return new boolean[]{ false, true };
    }

    /**
     * Check if this is a proper list.
     *
     * @return {@code true} if this is a proper list.
     */
    public boolean isProperList()
    {
        return consAttributes( new Holder<Long>( null ) )[1];
    }
    static public boolean isProper( Cons list )
    {
        return Cons.NIL == list ?
                true :
                list.isProperList();
    }

    /**
     * @return {@code true} if the passed list is circular.
     */
    public boolean isCircular()
    {
        return consAttributes( new Holder<Long>( null ) )[0];
    }

    /**
     * Return the length of the list represented by this Cons object.
     * Handles improper lists.
     *
     * @return The length of this list.
     * @throws RuntimeX If the list is circular.
     */
    public long length() throws RuntimeX
    {
        Holder<Long> lengthOut = new Holder<Long>( null );

        var pc = consAttributes( lengthOut );

        // Circular.
        if ( pc[0] )
            throw RuntimeX.mExpectedProperList();

        return lengthOut.get();
    }
    public static long length( Cons c ) throws RuntimeX
    {
        if ( c == Cons.NIL )
            return 0;
        return c.length();
    }

    /**
     * The length operation as required by the Scheme standard.
     * Accepts no improper lists.
     * @return The length of this list.
     * @throws RuntimeX If the list is not proper.
     */
    public long properLength() throws RuntimeX
    {
        Holder<Long> lengthOut = new Holder<Long>( null );

        var pc = consAttributes( lengthOut );

        // Not proper.
        if ( ! pc[1] )
            throw RuntimeX.mExpectedProperList();

        return lengthOut.get();
    }

    /**
     * Returns this {@code Cons} as an array.
     *
     * @return A java array containing the list's elements.
     * @throws RuntimeX
     */
    public FirstClassObject[] asArray() throws RuntimeX
    {
        Cons c = this;

        FirstClassObject[] argArray = new FirstClassObject[ (int)length() ];

        for ( int i = 0 ; i < argArray.length ; i++ )
        {
            argArray[i] = c._car;
            c = (Cons)c._cdr;
        }

        return argArray;
    }
    public static FirstClassObject[] asArray( Cons c ) throws RuntimeX
    {
        if ( c == Cons.NIL )
            return new FirstClassObject[0];

        return c.asArray();
    }

    /**
     * Converts the {@code Cons} to a Java {@code Object[]}.  All referenced
     * objects get recursively converted.
     *
     * @return The equivalent to a list in the Java type system.  This is a
     *         {@code FirstClassObject[]}.
     * @throws RuntimeX
     */
    @Override
    public Object toJava() throws RuntimeX
    {
        FirstClassObject[] unconverted = asArray();

        Object[] result = new Object[ unconverted.length ];

        for ( int i = 0 ; i < result.length ; i++ )
            result[i] = unconverted[i].toJava();

        return result;
    }

    /**
     * Set the constantness of this {@code Cons}.  The new value is
     * propagated to the referenced car and cdr nodes.  Note that it is not
     * possible to switch the constantness from constant to changeable.
     *
     * @param what The new value for the constantness.
     */
    @Override
    void setConstant( boolean what )
    {
        super.setConstant( what );
        setConstant( _car, what );
        setConstant( _cdr, what );
    }
}
