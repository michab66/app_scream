/* $Id: SchemeReader.java 209 2009-11-24 09:14:44Z Michael $
 *
 * Scream.
 *
 * Released under Gnu Public License
 * Copyright © 2009 Michael G. Binz
 */

package de.michab.scream;

import java.io.Reader;
import java.util.ArrayDeque;
import java.util.Deque;

import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;


/**
 * A Scheme parser frontend that manages a stack of real parsers
 * on certain Readers.  This is needed to be able to differ the
 * reading of Scheme source in potentially several files from
 * the actual interpretation.
 *
 * @version $Rev: 209 $
 * @author Michael Binz
 */
// TODO merge with parser?
public class SchemeReader
{
    private Deque<SchemeParser> _parseStack =
        new ArrayDeque<SchemeParser>();



    /**
     * Create a SchemeReader with an empty stack of parsers.
     */
    public SchemeReader()
    {
    }



    /**
     * Create a SchemeReader with an initial parser in the
     * passed Reader.
     *
     * @param in A Reader on a Scheme source file.
     */
    public SchemeReader( Reader in )
    {
        push( in );
    }



    /**
     * Get a single expression.
     *
     * @return An expression.
     * @throws FrontendX In case of an error.
     */
    public FirstClassObject getExpression()
      throws FrontendX
    {
        while ( true )
        {
            SchemeParser parser = _parseStack.peek();

            // Stack is empty.
            if ( null == parser )
                return Port.EOF;

            FirstClassObject result = parser.getExpression();

            if ( ! Port.EOF.equals( result ) )
                return result;

            _parseStack.pop();
        }
    }



    /**
     *
     * @param in
     */
    public void push( Reader in )
    {
      // TODO:  The passed readers are never closed.
        _parseStack.push( new SchemeParser( in ) );
    }
}
