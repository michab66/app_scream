/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX;
import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

public class FirstClassObjectTest extends ScreamBaseTest
{
    @Test
    public void as() throws Exception
    {
        FirstClassObject fco = SchemeString.make( "123" );

        assertEquals( 3, fco.as( SchemeString.class ).length() );
    }

    @Test
    public void as_fail() throws Exception
    {
        FirstClassObject fco = SchemeString.make( "123" );

        try
        {
            fco.as( Symbol.class ).toString();
            fail();
        }
        catch ( RuntimeX rx )
        {
            assertEquals( Code.TYPE_ERROR, rx.getCode() );
            assertEquals( SchemeString.make( "symbol" ), rx.getArgument(0) );
            assertEquals( SchemeString.make( "string=\"123\"" ), rx.getArgument(1) );
        }
    }
}
