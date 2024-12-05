/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.pops;

import de.michab.scream.fcos.Environment;

/**
 * Represents the per-thread interpreter registers.
 */
public class REGS
{
    private final static ThreadLocal<Environment> _CENV =
            new ThreadLocal<>();

    /**
     * @return The current environment
     */
    public static Environment CENV()
    {
        return _CENV.get();
    }
    public static void CENV( Environment environment )
    {
        _CENV.set( environment );
    }

    private REGS()
    {
        throw new AssertionError();
    }
}
