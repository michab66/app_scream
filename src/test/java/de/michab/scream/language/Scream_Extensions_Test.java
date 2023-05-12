/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.language;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import de.michab.scream.RuntimeX.Code;
import de.michab.scream.ScreamBaseTest;

/**
 * scream extensions
 */
public class Scream_Extensions_Test extends ScreamBaseTest
{
    @Test
    public void scream_eval_1() throws Exception
    {
        expectFco( "(scream:eval '(+ 300 10 3))", i313 );
    }
    @Test
    public void scream_eval_err_1() throws Exception
    {
        expectError( "(scream:eval 1 2)", Code.WRONG_NUMBER_OF_ARGUMENTS );
    }
    @Test
    public void scream_eval_err_2() throws Exception
    {
        expectError( "(scream:eval)", Code.WRONG_NUMBER_OF_ARGUMENTS );
    }
    @Test
    public void scream_eval_err_3() throws Exception
    {
        expectError( "(scream:eval '(1 2))", Code.CALLED_NON_PROCEDURAL );
    }
    @Disabled( "https://github.com/urschleim/scream/issues/157" )
    @Test
    public void scream_eval_err_4() throws Exception
    {
        expectError( "(scream:eval '(+ 1 2 3 4 5 6 7 8 9 'donald))", Code.TYPE_ERROR );
    }
}
