/* $Id: ConversionFailedX.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream kernel.
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;

import de.michab.scream.fcos.FirstClassObject;

/**
 * Exception is used in argument list conversion in SchemeObject.  This is
 * a convenience wrapper around the "TYPE_ERROR" and should be always used
 * instead of the code sequence {@code new RuntimeX( "TYPE_ERROR", ... )}.
 *
 * @version $Rev: 209 $
 * @author Michael Binz
 */
@SuppressWarnings("serial")
public class ConversionFailedX
extends RuntimeX
{
    /**
     * Creates an instance of this exception.
     *
     * @param actual The reference to the object that couldn't be converted.
     * @param formal The class that was tried to convert to.
     */
    public ConversionFailedX( FirstClassObject actual, Class<?> formal )
    {
        super( Code.TYPE_ERROR,
                formalName( formal ),
                FirstClassObject.getTypename( actual ) );
    }

    /**
     * Creates an instance of this exception including an optional position
     * argument.
     *
     * @param actual The reference to the object that couldn't be converted.
     * @param formal The class that was tried to convert to.
     * @param position The position of the error in a parameter list.
     */
    public ConversionFailedX( FirstClassObject actual, Class<?> formal, int position )
    {
        super( Code.TYPE_ERROR,
                formalName( formal ),
                FirstClassObject.getTypename( actual ),
                "" + position );
    }

    /**
     * Computes a formal type name for the passed class.  If the passed
     * {@code Class} instance is a {@code FirstClassObject} then the
     * {@code TYPE_NAME} attribute is returned.  If the passed class is not
     * part of Scream then the class name will be returned.
     * <br>
     * The sense behind of that is that the class
     * {@code de.michab.scream.SchemeString} is simply a {@code string}
     * when it comes to error messages.
     *
     * @param formal A reference to the class.
     * @return A name for the type represented by the {@code formal}
     *         argument.
     */
    private static String formalName( Class<?> formal )
    {
        String result = formal.getName();

        try
        {
            if ( FirstClassObject.class.isAssignableFrom( formal ) )
                result = (String)formal.getField( "TYPE_NAME" ).get( null );
        }
        catch ( Exception ignore )
        {
            // Since result is initialized we do not have to do any special here.
        }

        return result;
    }
}
