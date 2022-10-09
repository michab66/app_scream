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
