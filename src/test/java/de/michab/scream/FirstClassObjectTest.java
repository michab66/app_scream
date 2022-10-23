/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamException.Code;

public class FirstClassObjectTest extends TestUtil
{
    @Test
    public void asInteger() throws Exception
    {
        FirstClassObject fco = i313;

        SchemeInteger i = FirstClassObject.as( SchemeInteger.class, fco );
        assertEquals( 313L, i.asLong() );
    }

    @Test
    public void asIntegerNil() throws Exception
    {
        FirstClassObject fco = Cons.NIL;

        SchemeInteger i = FirstClassObject.as( SchemeInteger.class, fco );
        assertEquals( Cons.NIL, i );
    }

    @Test
    public void asSymbolNil() throws Exception
    {
        FirstClassObject fco = Cons.NIL;

        Symbol i = FirstClassObject.as( Symbol.class, fco );
        assertEquals( Cons.NIL, i );
    }

    @Test
    public void asIntegerFail() throws Exception
    {
        FirstClassObject fco = s313;

        try
        {
            FirstClassObject.as( SchemeInteger.class, fco );
            fail();
        }
        catch ( ScreamException e )
        {
            assertEquals( Code.TYPE_ERROR, e.getCode() );
            // Check for format error.
            assertNotNull( e.getMessage() );
        }
    }
}
