/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

package de.michab.scream;

import org.junit.jupiter.api.Test;

public class SymbolTest
{
    Symbol xx( String s )
    {
        return null;
    }

    @Test
    public void toJava() throws Exception
    {
        TestUtil.toJava_(
                Symbol.class,
                String.class,
                "micbinz",
                Symbol::createObject );
    }
}
