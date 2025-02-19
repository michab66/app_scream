/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.frontend;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Int;

public class FrontendNumbersTest
{
    private void validate(
            String scheme,
            long expected )
                    throws RuntimeX
    {
        Int n = ScreamBaseTest.parse(
                scheme,
                Int.class );
        assertEquals(
                expected,
                n.asLong() );
    }

    @Test
    public void parseIntegers() throws Exception
    {
        validate( "#x10", 16 );

        validate( "0", 0 );

        validate( "-1", -1 );
        validate( "+1", 1 );

        validate( "" + Long.MAX_VALUE, Long.MAX_VALUE );
        validate( "" + Long.MIN_VALUE, Long.MIN_VALUE );

        validate( "+" + Long.MAX_VALUE, Long.MAX_VALUE );
    }
}
