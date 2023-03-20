/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2009-2022 Michael G. Binz
 */
package de.michab.scream;

import java.io.Reader;
import java.util.ArrayDeque;
import java.util.Deque;

import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Port;
import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

/**
 * A Scheme parser frontend that manages a stack of real parsers
 * on certain Readers.  This is needed to be able to differ the
 * reading of Scheme source in potentially several files from
 * the actual interpretation.
 *
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
      throws RuntimeX
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
    private void push( Reader in )
    {
      // TODO:  The passed readers are never closed.
        _parseStack.push( new SchemeParser( in ) );
    }
}
