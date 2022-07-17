/* $Id: FrontendX.java 185 2009-06-21 12:30:22Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream.frontend;

import de.michab.scream.RuntimeX;



/**
 * A exception common for all frontend related errors thrown by the scanning
 * and parsing phases.  For a description of the <code>key</code> argument
 * used by all of the constructors see
 * {@link de.michab.scream.ScreamException ScreamException}.
 */
public class FrontendX
  extends RuntimeX
{
  /**
   * The input file's name.  Defaults to 'anonymous-file'.
   */
  private String _filename = "anonymous-file";



  /**
   * Creates a frontend exception.
   *
   * @param key  The message key.  See
   *             {@link de.michab.scream.ScreamException ScreamException}
   *             for further explanation of message key resolution.
   */
  public FrontendX( String key )
  {
    super( key );
  }



  /**
   * Creates a frontend exception.  The <code>line</code>, <code>column</code>
   * and <code>yytext</code> parameters are internally added to the argument
   * list used in exception resolution and message generation.
   *
   * @param line The error's line number.  Line counting begins with 1.
   * @param column The error's column number.  Column counting begins with 1.
   * @param yytext The offending part of the source file.
   * @param key  The message key.  See
   *             {@link de.michab.scream.ScreamException ScreamException}
   *             for further explanation of message key resolution.
   */
  public FrontendX( int line, int column, String key, String yytext )
  {
    super( key, new Object[]{ "" + line, "" + column, yytext } );
  }



  /**
   * Creates a frontend exception.  The <code>line</code> and
   * <code>column</code> parameters are internally added to the argument
   * list used in exception resolution and message generation.
   *
   * @param line The error's line number.  Line counting begins with 1.
   * @param column The error's column number.  Column counting begins with 1.
   * @param key  The message key.  See
   *             {@link de.michab.scream.ScreamException ScreamException}
   *             for further explanation of message key resolution.
   */
  public FrontendX( int line, int column, String key )
  {
    super( key, new Object[]{ "" + line, "" + column } );
  }



  /**
   * Creates a frontend exception.  The passed argument list is used in
   * exception resolution and message generation.
   *
   * @param key  The message key.  See
   *             {@link de.michab.scream.ScreamException ScreamException}
   *             for further explanation of message key resolution.
   * @param args The exception's arguments.
   */
  public FrontendX( String key, Object[] args )
  {
    super( key, args );
  }



  /**
   * Set the filename for this exception.
   *
   * @param filename The filename to use.
   */
  public void setFilename( String filename )
  {
    _filename = filename;
  }



  /**
   * Get the name of the source file.  If that hasn't been set via
   * <code>setFilename()</code> the name 'anonymous-file' is returned.
   *
   * @return The name of the source file.
   * @see #setFilename
   */
  public String getFilename()
  {
    return _filename;
  }
}
