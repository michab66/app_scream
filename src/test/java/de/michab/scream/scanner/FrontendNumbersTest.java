/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.scanner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.junit.jupiter.api.Test;

import de.michab.scream.SchemeInteger;
import de.michab.scream.ScreamException;
import de.michab.scream.frontend.FrontendX;
import de.michab.scream.frontend.SchemeParser;

public class FrontendNumbersTest
{
    private void validate(
            String scheme,
            long expected )
                    throws FrontendX
    {
        var fco =
                new SchemeParser( scheme ).getExpression();
        assertInstanceOf( SchemeInteger.class, fco );
        SchemeInteger n = (SchemeInteger)fco;

        assertEquals( expected, n.asLong() );
    }

    @Test
    public void parseIntegers() throws ScreamException
    {
        validate( "1", 1 );
        validate( "0", 0 );

        validate( "-1", -1 );
        validate( "+1", 1 );

        validate( "" + Long.MAX_VALUE, Long.MAX_VALUE );
        validate( "" + Long.MIN_VALUE, Long.MIN_VALUE );

        validate( "+" + Long.MAX_VALUE, Long.MAX_VALUE );
    }
}
