/* $Id: Port.java 788 2015-01-10 23:07:11Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;

import org.smack.util.FileUtil;

import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;



/**
 * Represents a scheme port object.  Ports represent input and output devices.
 * To Scheme, an input port is a Scheme object that can deliver characters
 * upon command, while an output port is a Scheme object that can accept
 * characters.
 *
 * @version $Rev: 788 $
 * @author Michael Binz
 */
public class Port
  extends FirstClassObject
{
  // TODO This class should be split in two classes InputPort and OutputPort.
  // These represent completely different SchemeTypes.
  // Several other major issues exist with this implementation:
  //
  // (1) The load operation should propagate the current directory.  That
  //     means if a file that is loaded in turn contains a load operation, the
  //     file should be resolved relative to the current file.  Not clear
  //     *how* to do that.
  //
  // (2) Reading/writing to the console ports results in strange behavior.  An
  //     explicit lockout should be implemented.  Not clear *how* to do that.



  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#typename()
   */
  public static final String TYPE_NAME = "port";



  /**
   * This Port's type.
   */
  private final int _type;



  public static final int Output = 0;
  public static final int Input = 1;



  /**
   * An object representing EOF.
   */
  public static final Symbol EOF = Symbol.createObject( "EOF" );



 /**
   * This is the name for the port.  Default name is 'anonymous'.
   */
   private final String _name;



  /**
   * The java object representing an input port.
   */
  private Reader _inPort = null;



  /**
   * The java object representing an output port.
   */
  private Writer _outPort = null;



  /**
   * The parser used solely by the read method.
   */
  private SchemeParser _parser = null;



  private static final int NOT_PEEKED = -2;



  /**
   *
   */
  private int _peeked = NOT_PEEKED;



  /**
   * Create an output port.  The name that is passed is used for generating
   * correct file references in case of errors.
   *
   * @param name A name to use for this port.
   * @param out A writer that output gets written to.
   */
  public Port( String name, Writer out )
  {
    _type = Output;
    _name = name;
    _outPort = out;
  }



  /**
   * Create an input port.
   *
   * @param name A name to use for this port.
   * @param in The reader to use in this port.
   */
  public Port( String name, Reader in )
  {
    _type = Input;
    _name = name;
    _inPort = in;
  }



  /**
   * Create a named port.  The passed name is used for opening the file.
   *
   * @param name The name of the file to open.
   * @param inout An enum value defining whether this is an input or output
   *        file.
   * @throws RuntimeX If an error occurred.
   */
  public Port( String name, int inout )
    throws RuntimeX
  {
    _type = inout;
    _name = name;

    try
    {
      if ( Input == inout )
      {
        _inPort = new InputStreamReader(
                    new FileInputStream( name ) );
      }
      else if ( Output == inout )
      {
        _outPort = new PrintWriter(
                     new FileOutputStream( name ) );
      }
      else
        throw new RuntimeX( "INTERNAL_ERROR",
                            new Object[]{ getClass().getName() } );
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }
  }



  /**
   * Create a named port.  The passed name is used for opening the file.
   *
   * @param name The URL to open.
   * @param inout An enum value defining whether this is an input or output
   *        file.
   * @throws RuntimeX If an error occurred.
   */
  public Port( java.net.URL name, int inout )
    throws RuntimeX
  {
    _type = inout;
    _name = name.toExternalForm();

    try
    {
      if ( Input == inout )
      {
        _inPort = new InputStreamReader( name.openStream() );
      }
      else if ( Output == inout )
      {
        _outPort = new PrintWriter( name.openConnection().getOutputStream() );
      }
      else
        throw new RuntimeX( "INTERNAL_ERROR",
                            new Object[]{ getClass().getName() } );
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }
  }



  /**
   * Return a port's string representation.
   *
   * @return A string representation for this port.
   */
  @Override
public String toString()
  {
    StringBuilder result = new StringBuilder( "<" );
    result.append( isInputPort() ? "input" : "output" );
    result.append( "-port " );
    result.append( _name );
    result.append( ">" );

    return result.toString();
  }



  /**
   * Tests whether this is an input port.
   *
   * @return <code>true</code> if this is an input port, <code>false</code>
   * otherwise.
   */
  public boolean isInputPort()
  {
    return _type == Input;
  }



  /**
   * Tests whether this is an output port.
   *
   * @return <code>true</code> if this is an output port, <code>false</code>
   * otherwise.
   */
  public boolean isOutputPort()
  {
    return _type == Output;
  }


  /**
   * Tests if a port is closed.
   *
   * @return <code>true</code> if the port is closed, <code>false</code> if the
   * port is open.
   */
  public boolean isClosed()
  {
    return (_outPort == null) && (_inPort == null);
  }



  /**
   * Write the passed object to the port.
   *
   * @param o The object to write.
   * @param flush <code>True</code> results in the port being flushed after the
   *        data is written.
   * @throws RuntimeX In case an error ocurred.
   */
  public void write( FirstClassObject o, boolean flush )
    throws RuntimeX
  {
    write( toString( o ), flush );
  }



  /**
   * Write the passed object to the port.
   *
   * @param o The object to write.
   * @throws RuntimeX In case an error ocurred.
   */
  public void write( FirstClassObject o )
    throws RuntimeX
  {
    write( o, false );
  }



  /**
   * Write a string to the port.  This writes the external string
   * representation including quotes.
   *
   * @param s The string to write.
   * @param flush If <code>true</code> then port is flushed after the write.
   * @throws RuntimeX In case an error occurred.
   */
  void write( String s, boolean flush )
    throws RuntimeX
  {
    if ( !isOutputPort() )
      throw new RuntimeX( "EXPECTED_OUTPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    try
    {
      _outPort.write( s );
      _outPort.write( ' ' );
      if ( flush )
        _outPort.flush();
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }
  }



  /**
   * Write a single character to this output port.
   *
   * @param c The character to write.
   * @throws RuntimeX In case something went wrong.
   */
  public void writeCharacter( char c )
    throws RuntimeX
  {
    // Check preconditions.
    if ( !isOutputPort() )
      throw new RuntimeX( "EXPECTED_OUTPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    // Do the actual operation.
    try
    {
      _outPort.write( c );
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }
  }



  /**
   * Write a human readable string for this FCO.  Human readable means that the
   * double quotes and quotes are not printed.
   *
   * @param o The object to display.
   * @throws RuntimeX In case an error occurred.
   */
  public void display( FirstClassObject o )
    throws RuntimeX
  {
    if ( o instanceof SchemeString )
      write( ((SchemeString)o).getValue(), true );
    else if ( o instanceof SchemeCharacter )
      write( "" + ((SchemeCharacter)o).asCharacter(), true );
    else
      write( o );
  }



  /**
   * Read a single first class object from this port.  After the read the port
   * is positioned on the first character after this data element.
   *
   * @return The next datum encountered on the input port.
   * @throws RuntimeX In case of an error.
   */
  public FirstClassObject read()
    throws RuntimeX
  {
    if ( !isInputPort() )
      throw new RuntimeX( "EXPECTED_INPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    // In case no parser exists...
    if ( null == _parser )
      // ...create one.
      _parser = new SchemeParser( _inPort );

    try
    {
      return _parser.getExpression();
    }
    catch ( FrontendX e )
    {
      e.setFilename( _name );
      throw e;
    }
  }



  /**
   * Checks whether a character is ready to be read from this port without
   * blocking.
   *
   * @return <code>True</code> if a character can be read without blocking.
   * @throws RuntimeX In case an error occurred.
   */
  public boolean charReady()
    throws RuntimeX
  {
    // Do the needed checks.
    if ( !isInputPort() )
      throw new RuntimeX( "EXPECTED_INPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    try
    {
      return _inPort.ready();
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }
  }



  /**
   * Read a single character from the port without advancing the file position.
   * The next <code>peekCharacter()</code> or <code>readCharacter()</code>
   * operation will just return the same character.  In case there are no
   * characters available this call will block.
   *
   * @return The peeked character.
   * @throws RuntimeX In case an error occurred.
   * @see #readCharacter
   */
  public FirstClassObject peekCharacter()
    throws RuntimeX
  {
    // Do the needed checks.
    if ( !isInputPort() )
      throw new RuntimeX( "EXPECTED_INPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    // Perform the actual read.

    if ( _peeked == NOT_PEEKED )
    {
      try
      {
        _peeked = _inPort.read();
      }
      catch ( IOException e )
      {
        throw new RuntimeX( "IO_ERROR",
                            new Object[]{ e.getMessage() } );
      }
    }

    // Compute result.
    if ( _peeked == -1 )
      return EOF;
    else
      return SchemeCharacter.createObject( _peeked );
  }



  /**
   * Read a character from this port.
   *
   * @return The character read or the end of file object.
   * @throws RuntimeX In case an error occurs.
   */
  public FirstClassObject readCharacter()
    throws RuntimeX
  {
    // Do the needed checks.
    if ( !isInputPort() )
      throw new RuntimeX( "EXPECTED_INPUT_PORT" );
    if ( isClosed() )
      throw new RuntimeX( "PORT_CLOSED" );

    // Perform the actual read.

    int c;
    if ( _peeked != NOT_PEEKED )
    {
      c = _peeked;
      _peeked = NOT_PEEKED;
    }
    else try
    {
      c = _inPort.read();
    }
    catch ( IOException e )
    {
      throw new RuntimeX( "IO_ERROR",
                          new Object[]{ e.getMessage() } );
    }

    // Compute result.
    if ( c == -1 )
      return EOF;
    else
      return SchemeCharacter.createObject( c );
  }

  /**
   * Closes this port.  This is also done from the finalizer.
   */
  public void close()
  {
      FileUtil.forceClose( _inPort );
      _inPort = null;
      FileUtil.forceClose( _outPort );
      _outPort = null;
  }

  /**
   * Finalize the object.
   *
   * @throws Throwable In case of errors.
   * @see java.lang.Object#finalize
   */
  @Override
  protected void finalize()
          throws
          Throwable
  {
      close();
      // Chain finalisers.
      super.finalize();
  }

  /**
   * Convert this object into the Java type system.  For input ports returns a
   * reader, for output ports a writer.
   *
   * @return The corresponding Java type for this object.
   */
  @Override
  public Object toJava()
  {
    if ( isInputPort() )
      return _inPort;
    else
      return _outPort;
  }

  /**
   * Extends the passed environment with the procedures logically associated
   * with this class.
   *
   * @param tle The environment to extend.
   * @return The passed environment after extension.
   */
  static Environment extendTopLevelEnvironment( Environment tle )
  {
    tle.define( EOF, EOF );

    return tle;
  }
}
