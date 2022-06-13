/* $Id: SchemeParser.java 172 2009-03-19 21:21:48Z Michael $
 *
 * Scream / Frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2009 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

public class SchemeParserTest
{
    @Test
    public void basic(){
        try {
            // Missing closing brace.
            SchemeParser sp = new SchemeParser( "(+ 300 13" );
            var x = sp.getExpression();
            fail();
        }
        catch( FrontendX e )
        {
            // Unexpected end of input.
            assertEquals( 37, e.getId() );
        }
    }
}
