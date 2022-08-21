package de.michab.scream;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class ScreamExceptionTest
{
    @Test
    public void basic() throws Exception
    {
        ScreamException se = new ScreamException( ScreamException.Code.INTERNAL_ERROR );

        assertEquals( -1, se.getId() );
    }
}
