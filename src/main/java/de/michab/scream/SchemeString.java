/* $Id: SchemeString.java 174 2009-03-21 18:37:23Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2002 Michael G. Binz
 */
package de.michab.scream;



/**
 * Represents the scheme string type.  Strings are sequences of characters.
 * Strings are written as sequences of characters enclosed within double quotes
 * (").  A double quote can be written inside a string only by escaping it with
 * a backslash (\), as in "The word \"recursion\" has many meanings."
 *
 * @version $Rev: 174 $
 * @author Michael Binz
 */
public class SchemeString
  extends FirstClassObject
{
  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#getTypename()
   */
  public static final String TYPE_NAME = "string";



  /**
   * This string's value.
   */
  private final StringBuilder _value;



  /**
   * Create a new string in a specified length.  The string is filled with
   * spaces.
   *
   * @param length The length for the new string.
   */
  public SchemeString( int length )
  {
    this( length, SchemeCharacter.SPACE.asCharacter() );
  }

  /**
   * Create a new string in a specified length, initialised with the passed
   * character.
   *
   * @param length The length for the new string.
   * @param filler A character used to fill the new string.
   */
  public SchemeString( int length, char filler )
  {
    _value = new StringBuilder( length );
    // Note: After creating this StringBuffer with a length, only the capacity
    // is set to length.  Before one can access indices between 0 and length-1
    // the string length has to be set.  Not very intuitive.
    _value.setLength( length );
    // At last fill the new string with the specified character.
    uncheckedFill( filler );
  }

  /**
   * Create a constant <code>SchemeString</code> from a Java string object.
   * Note that the input is expected to contain literal character
   * representations like '\n' and '\"'.  These will be replaced by the
   * respective character.  The <code>toString()</code> method can be used to
   * get again the same string representation as passed into this constructor
   * (but not the <i>same</i> string object with regards to object identity.)
   *
   * @param s A prototype string for the new object.
   */
  public SchemeString( String s )
  {
    this( s, true );
  }



  /**
   * Create a SchemeString from a java string object.  Note that the input is
   * expected to contain literal character representations like '\n' and '\"'.
   * These will be replaced by the respective character.  The
   * <code>toString()</code> method can be used to get again the same string
   * representation as passed into this constructor (but not the <i>same</i>
   * string object with regards to object identity.)
   *
   * @param s A prototype string for the new object.
   * @param constant Defines whether the created string represents a constant.
   */
  public SchemeString( String s, boolean constant )
  {
    // TODO(MB) We are not symmetric currently with the toString method.
    // We should accept here a valid Scheme string literal like
    // '"Düsenjäger\n"' including double quotes.
    _value = new StringBuilder( s );
    replaceAll( _value, "\\n", "\n" );
    replaceAll( _value, "\\\"", "\"" );
    setConstant( constant );
  }



  /**
   * Get this object's value as a Java string.  This is the actual string
   * value, not the literal string representation, i.e. no escaped characters
   * like '\"' are contained in the returned string.
   *
   * @return The value of the string.
   */
  public String getValue()
  {
    return _value.toString();
  }



  /**
   * Get this string's length.
   *
   * @return This string's length.
   */
  public int length()
  {
    return _value.length();
  }



  /**
   * Set the character at the specified position.  This is only allowed for
   * non-constant strings.
   *
   * @param idx The position to set the character at.  Has to be in range 0
   *            to this.length()-1.
   * @param character The character to set.
   * @throws RuntimeX If the index is out of bounds or the string was constant.
   */
  public void setCharAt( int idx, char character )
    throws RuntimeX
  {
    if ( isConstant() )
      throw new RuntimeX( "CANT_MODIFY_CONSTANT", new Object[]{ _value } );

    try
    {
      _value.setCharAt( idx, character );
    }
    catch ( StringIndexOutOfBoundsException e )
    {
      throw new RuntimeX( "INDEX_OUT_OF_BOUNDS", new Object[]{ "" + idx } );
    }
  }



  /**
   * Get the character at the specified position.
   *
   * @param idx The index to read.
   * @return The character from the specified position.
   * @throws RuntimeX In case the index is out of bounds.
   */
  public char getCharAt( int idx )
    throws RuntimeX
  {
    try
    {
      return _value.charAt( idx );
    }
    catch ( StringIndexOutOfBoundsException e )
    {
      throw new RuntimeX( "INDEX_OUT_OF_BOUNDS", new Object[]{ "" + idx } );
    }
  }



  /**
   * Return a substring.  The passed start index has to be smaller than the
   * passed end index.
   *
   * @param start The start index of the substring in range 0..length-1
   * @param end The end index of the substring in range 0..length-1
   * @return The requested substring.
   * @throws RuntimeX In case one of the indices were wrong.
   */
  public SchemeString substring( int start, int end )
    throws RuntimeX
  {
    String crtValue = "" + _value;

    try
    {
      return new SchemeString( crtValue.substring( start, end ) );
    }
    catch ( StringIndexOutOfBoundsException e )
    {
      throw new RuntimeX( "INDEX_OUT_OF_BOUNDS", new Object[]{ "" + end } );
    }
  }



  /**
   * Creates a new <code>SchemeString</code> from the current one with the
   * argument String appended.
   *
   * @param other The string to append.
   * @return The concatenated string.
   */
  public SchemeString append( SchemeString other )
  {
    if ( (FirstClassObject)other != Cons.NIL )
    {
      StringBuffer combined = new StringBuffer( getValue() );
      combined.append( other.getValue() );
      return new SchemeString( combined.toString(), false );
    }
    else
      return this;
  }



  /**
   * Compare this <code>SchemeString</code> with the passed string.
   *
   * @param other The string to compare with.
   * @return a negative integer, zero, or a positive integer as the specified
   *         String is greater than, equal to, or less than this String.
   *         Note that this is not what is expected, but compliant with
   *         <code>String.compareTo()</code>.
   */
  public int compareTo( SchemeString other )
  {
    return getValue().compareTo( other.getValue() );
  }



  /**
   * Compare this <code>SchemeString</code> with the passed string, ignoring
   * case considerations.
   *
   * @param other The string to compare with.
   * @return a negative integer, zero, or a positive integer as the specified
   *         String is greater than, equal to, or less than this String,
   *         ignoring case considerations.  Note that this is not what is
   *         expected, but compliant with
   *         <code>String.compareIgnoreCase</code>.
   */
  public int compareToIgnoreCase( SchemeString other )
  {
    return getValue().compareToIgnoreCase( other.getValue() );
  }



  /**
   * Create a list of characters from this string.
   *
   * @return The character list.
   */
  public Cons toCons()
  {
    FirstClassObject[] resultArray = new FirstClassObject[ length() ];

    for ( int i = resultArray.length -1 ; i >= 0 ; i-- )
      resultArray[i] = SchemeCharacter.createObject( _value.charAt( i ) );

    return Cons.create( resultArray );
  }



  /**
   * Fill the string with the passed character.  This method is private and
   * does <i>not</i> check the string's constantness.
   *
   * @param filler The character to use for filling the entire string.
   */
  private void uncheckedFill( char filler )
  {
    for ( int i = _value.length() -1 ; i >= 0 ; i-- )
      _value.setCharAt( i, filler );
  }



  /**
   * Fills the string with the passed character.  This is only allowed for
   * non-constant strings.
   *
   * @param filler The character to use for filling the entire string.
   * @throws RuntimeX In case the string was marked as constant.
   * @see de.michab.scream.FirstClassObject#isConstant
   */
  public void fill( char filler )
    throws RuntimeX
  {
    if ( isConstant() )
      throw new RuntimeX( "CANT_MODIFY_CONSTANT", new Object[]{ _value } );

    uncheckedFill( filler );
  }



  /**
   * The implementation of the scheme equal? procedure.  This is the least
   * efficient one since lists and arrays are deep compared. For other types
   * eqv? is used.
   *
   * @param other The string to compare with this string.
   * @return <code>true</code> if the passed string is equal to this string.
   */
  @Override
public boolean equal( FirstClassObject other )
  {
    if ( other == Cons.NIL )
      return false;

    try
    {
      return getValue().equals( ((SchemeString)other).getValue() );
    }
    catch ( ClassCastException e )
    {
      return false;
    }
  }



  /**
   * This returns the literal representation of the string object in Scheme
   * syntax.  Use the <code>getValue()</code> for the physical string.
   *
   * @return A literal representation of the string.
   * @see SchemeString#getValue
   * @see FirstClassObject#toString
   */
  @Override
public String toString()
  {
    return "\"" + toStringLiteral() + "\"";
  }



  /**
   * Creates a literal representation of the string.  That means double quotes
   * and carriage returns are represented as '\"' and '\n' in the resulting
   * string.
   *
   * @return A literal notation of the string.
   */
  private String toStringLiteral()
  {
    StringBuilder result = new StringBuilder( _value.toString() );
    replaceAll( result, "\n", "\\n" );
    replaceAll( result, "\"", "\\\"" );

    return result.toString();
  }


  /**
   * Searches through a passed string and replaces all occurrences of a given
   * string by another string.  Note that beginning with JDK1.4 there's a
   * method <code>replaceAll()</code> that provides the functionality that we
   * implemented here.  As soon as we move to that JDK version this method
   * should be used.
   *
   * @param buffer The subject of the replacement operation.  Note that this
   *               parameter is modified by this method.
   * @param toReplace The string that is replaced in the passed buffer.
   * @param replacement The replacement used.
   * @return Number of replacements done.
   */
  private int replaceAll( StringBuilder buffer,
                          String toReplace,
                          String replacement )
  {
    String subject = buffer.toString();

    int count = 0;
    int lenToReplace = toReplace.length();
    int i = subject.length();

    while ( 0 <= (i = subject.lastIndexOf( toReplace, i ) ) )
    {
      buffer.replace( i, i + lenToReplace, replacement );
      // Note that we're cool here.  Even if 'i' is 0 and we decrement it
      // to -1 the String.lastIndexOf call above allows that explicitly.
      i--;
      count++;
    }

    return count;
  }

  /**
   * Creates a clone of a given <code>SchemeString</code> object.  Note that
   * the returned string is not constant.
   *
   * @return A clone of this object.
   * @see FirstClassObject#clone
   */
  @Override
  public SchemeString copy()
  {
    return new SchemeString( getValue(), false );
  }

  /**
   * Converts this this Scheme string object to a corresponding
   * <code>java.lang.String</code> object.
   *
   * @return An object representing this object in the Java type system.
   */
  @Override
public Object toJava()
  {
    return _value.toString();
  }
}
