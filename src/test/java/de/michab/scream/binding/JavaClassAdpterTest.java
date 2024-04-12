/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.binding;

import org.junit.jupiter.api.Test;

import de.michab.scream.ScreamBaseTest;
import de.michab.scream.fcos.Operation;
import de.michab.scream.fcos.Procedure;
import de.michab.scream.util.Scut;

public class JavaClassAdpterTest extends ScreamBaseTest
{
    @Test
    public void exists() throws Exception
    {
        var t = makeTester();

        Scut.as( Operation.class,
                t.execute(
                        "object" ) );
        Scut.as( Procedure.class,
                t.execute(
                        "object?" ) );
        Scut.as( Operation.class,
                t.execute(
                        "make-object" ) );
    }

}