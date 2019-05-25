/* $Id: SchemeBoolean.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream / Kernel
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2000 Michael G. Binz
 */
package de.michab.scream;



/**
 * Represents the Scheme boolean type.  The following is the description from
 * the Scheme R5 spec:  The standard boolean objects for true and false are
 * written as <code>#t</code> and <code>#f</code>. What really matters, though,
 * are the objects that the Scheme conditional expressions (if, cond, and, or,
 * do) treat as true or false. The phrase <i>a true value</i> or sometimes just
 * <i>true</i>) means any object treated as true by the conditional
 * expressions, and the phrase <i>a false value</i> (or <i>false</i>) means any
 * object treated as false by the conditional expressions.<br>
 * Of all the standard Scheme values, only #f counts as false in conditional
 * expressions. Except for #f, all standard Scheme values, including #t, pairs,
 * the empty list, symbols, numbers, strings, vectors, and procedures, count as
 * true.
 *
 * @version $Rev: 209 $
 * @author Michael Binz
 */
public class SchemeBoolean
  extends FirstClassObject
{
  /**
   * The name of the type as used by error reporting.
   *
   * @see FirstClassObject#getTypename()
   */
  public static final String TYPE_NAME = "boolean";



  /**
   * The single true value.
   */
  public static final SchemeBoolean T = new SchemeBoolean();



  /**
   * The single false value.
   */
  public static final SchemeBoolean F = new SchemeBoolean();



  /**
   * A factory for scheme booleans.
   */
  static public SchemeBoolean createObject( boolean v )
  {
    return v ? T : F;
  }



  /**
   * Constructor is private.  Access only allowed via the static members T/F.
   */
  private SchemeBoolean()
  {
    setConstant( true );
  }



  /**
   * Access the boolean's value as a primitive java type.
   *
   * @return The primitive value.
   * @see SchemeBoolean#convertToJava() for the wrapped value.
   */
  public boolean getValue()
  {
    return this == T;
  }



  /**
   * Returns a string representation of this object.
   *
   * @return A string representation of this object.
   */
  public String toString()
  {
    return this == T ? "#T" : "#F";
  }



  /**
   * Convert this instance into the native Java type system.  Booleans are
   * represented by <code>Boolean.TRUE</code> and <code>Boolean.FALSE</code>.
   *
   * @return <code>Boolean.TRUE</code> or <code>Boolean.FALSE</code> depending
   *         on the object's value.
   * @see SchemeBoolean#getValue() for access to the primitive value.
   */
  public Object convertToJava()
  {
    return this == T ? Boolean.TRUE : Boolean.FALSE;
  }
}
