/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

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

        SchemeInteger i = Scut.as( SchemeInteger.class, fco );
        assertEquals( 313L, i.asLong() );
    }
}
