/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import de.michab.scream.fcos.Cons;
import de.michab.scream.fcos.FirstClassObject;
import de.michab.scream.fcos.Int;
import de.michab.scream.fcos.Symbol;
import de.michab.scream.util.Scut;

public class FirstClassObjectTest extends ScreamBaseTest
{
    /**
     * Placeholder.
     * @throws Exception
     */
    @Test
    public void asInteger() throws Exception
    {
        FirstClassObject fco = i313;

        Int i = Scut.as( Int.class, fco );
        assertEquals( 313L, i.asLong() );
    }

    @Test
    public void is() throws Exception
    {
        assertTrue( FirstClassObject.is( Cons.class, Cons.NIL ) );
        assertTrue( FirstClassObject.is( Symbol.class, s("gaukeley") ) );

        assertFalse( FirstClassObject.is( Symbol.class, Cons.NIL ) );
        assertFalse( FirstClassObject.is( Symbol.class, i(313) ) );
    }
}
