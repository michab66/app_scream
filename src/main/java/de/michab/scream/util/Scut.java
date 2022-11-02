package de.michab.scream.util;

import java.util.HashSet;

import de.michab.scream.Cons;
import de.michab.scream.FirstClassObject;
import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamException.Code;

/**
 * Scream utilities.
 *
 * @author micbinz
 */
public class Scut
{
    public static void checkUnique( Cons c ) throws RuntimeX
    {
        var unifier = new HashSet<FirstClassObject>();
        checkUnique( unifier, c );
    }

    public static void checkUnique(
            HashSet<FirstClassObject> unifier,
            Cons c ) throws RuntimeX
    {
        while ( true )
        {
            var car = c.getCar();
            var cdr = c.getCdr();

            if ( ! unifier.add( car ) )
                throw mDuplicateElement( car );

            if ( Cons.NIL == cdr )
                break;
            if ( cdr instanceof Cons )
            {
                c = (Cons)cdr;
                continue;
            }

            if ( ! unifier.add( cdr ) )
                throw mDuplicateElement( cdr );
            break;
        }
    }

    //  #
    //  # 0: Name of expected type
    //  # 1: Name of actual type
    //  # 2: Optional: Position of wrong parameter in a parameter list.
    //  #
    //  TYPE_ERROR_2 = \
    //  11 : Argument has wrong type.  Expected {0} but found {1}.
    //  TYPE_ERROR_3 = \
    //  11 : Argument {2} has wrong type.  Expected {0} but found {1}.

    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError(
            Class<T1> expected,
            Class<T2> actual )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.typename( actual ) );
    }
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mTypeError(
            Class<T1> expected,
            Class<T2> actual,
            int position )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.TYPE_ERROR,
                FirstClassObject.typename( expected ),
                FirstClassObject.typename( actual ),
                position );
    }

    //  #
    //  # A list contained a duplicate element.  Used in case-syntax.
    //  #
    //  DUPLICATE_ELEMENT_1 = \
    //  46 : Duplicate element : {0}
    public static  <T1 extends FirstClassObject, T2 extends FirstClassObject>
    RuntimeX mDuplicateElement(
            FirstClassObject duplicate )
                    throws RuntimeX
    {
        return new RuntimeX(
                Code.DUPLICATE_ELEMENT,
                FirstClassObject.toString( duplicate ) );
    }

}
