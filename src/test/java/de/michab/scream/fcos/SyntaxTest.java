/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2023 Michael G. Binz
 */
package de.michab.scream.fcos;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.pops.SyntaxLet;

public class SyntaxTest extends ScreamBaseTest
{
    @Test
    public void basic() throws Exception
    {
        assertEquals( "letrec", SyntaxLet.letrecSyntax.getName().toString() );
    }
}
