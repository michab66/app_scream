/* $Id: ConversionFailedX.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream kernel.
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;



/**
 * Exception is used in argument list conversion in SchemeObject.  This is
 * a convenience wrapper around the "TYPE_ERROR" and should be always used
 * instead of the code sequence <code>new RuntimeX( "TYPE_ERROR", ... )</code>.
 *
 * @version $Rev: 209 $
 * @author Michael Binz
 */
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
    super( "TYPE_ERROR",
           new Object[]
           {
              formalName( formal ),
              FirstClassObject.getTypename( actual )
           } );
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
    super( "TYPE_ERROR",
           new Object[]
           {
              formalName( formal ),
              FirstClassObject.getTypename( actual ),
              "" + position
           } );
  }



  /**
   * Computes a formal type name for the passed class.  If the passed
   * <code>Class</code> instance is a <code>FirstClassObject</code> then the
   * <code>TYPE_NAME</code> attribute is returned.  If the passed class is not
   * part of Scream then the class name will be returned.<br>
   * The sense behind of that is that the class
   * <code>de.michab.scream.SchemeString</code> is simply a <code>string</code>
   * when it comes to error messages.
   *
   * @param formal A reference to the class.
   * @return A name for the type represented by the <code>formal</code>
   *         argument.
   */
  private static String formalName( Class formal )
  {
    String result = formal.getName();

    try
    {
      if ( FirstClassObject.class.isAssignableFrom( formal ) )
        result = (String)formal.getField( "TYPE_NAME" ).get( null );
    }
    catch ( Exception e )
    {
      // Since result is initialized we do not have to do any special here.
    }

    return result;
  }
}
