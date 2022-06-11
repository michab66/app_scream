/* $Id: RuntimeX.java 194 2009-08-03 20:26:36Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2002 Michael G. Binz
 */
package de.michab.scream;



/**
 * An exception to be thrown at run-time of a scheme program.  This type of
 * exception gets transformed to a user error message.
 *
 * @version $Rev: 194 $
 * @author Michael Binz
 */
public class RuntimeX
  extends ScreamException
{
  /**
   * Create an error message with parameters.
   *
   * @param msg The access key into the error message resource bundle.
   * @param args The arguments to be formatted into the error message.
   */
  public RuntimeX( String msg, Object[] args )
  {
    super( msg, args );
  }



  /**
   * Create an error message with parameters.
   *
   * @param msg The access key into the error message resource bundle.
   */
  public RuntimeX( String msg )
  {
    super( msg );
  }



  /**
   * (%error-catch expression)
   */
  static private Syntax errorCatchSyntax = new Syntax( "%error-catch" )
  {
    public FirstClassObject activate( Environment parent, FirstClassObject[] args )
      throws RuntimeX
    {
      checkArgumentCount( 1, args );

      try
      {
        return FirstClassObject.evaluate( args[0], parent );
      }
      catch ( RuntimeX e )
      {
        return SchemeInteger.createObject( e.getId() );
      }
    }
  };



  /**
   * Scream specific <code>(error ...)</code> procedure.  Interrupts the
   * current computation with a runtime error.
   */
  static private Procedure errorProcedure = new Procedure( "error" )
  {
    protected FirstClassObject apply( Environment parent, FirstClassObject[] args )
      throws RuntimeX
    {
      checkMinimumArgumentCount( 1, args );
      checkArgument( 1, SchemeString.class, args[0] );

      // Yes, the first argument is definitely a SchemeString.
      String message = createReadable( args[0] );

      // Transform the remaining arguments in error arguments.
      Object[] arguments = new Object[ args.length -1 ];
      for ( int i = args.length-1 ; i > 0 ; i-- )
        arguments[i-1] = createReadable( args[i] );

      // We must have been called by a scheme-defined procedure.  Get its name
      // and report that as the operation in error.
      RuntimeX result = new RuntimeX( message, arguments );
      result.setOperationName( parent.getName() );
      throw result;
    }



    /**
     * Makes a human readable string from a FirstClassObject.  That means for
     * a real scheme string that the double quotes are removed -- gnah instead
     * of "gnah" -- and that for all other cases the FCO.stringize is called,
     * handling with grace even NIL.
     */
    private String createReadable( FirstClassObject o )
    {
      String result;

      if ( o instanceof SchemeString )
        result = ((SchemeString)o).getValue();
      else
        result = FirstClassObject.stringize( o );

      return result;
    }
  };



  /**
   * This is just a standard extendTopLevelEnvironment method as on other
   * Scream classes.
   *
   * @param tle The top-level environment to be extended.
   */
  static Environment extendTopLevelEnvironment( Environment tle )
  {
    // Add our local definitions.
    tle.setPrimitive( errorProcedure );
    tle.setPrimitive( errorCatchSyntax );
    return tle;
  }
}
