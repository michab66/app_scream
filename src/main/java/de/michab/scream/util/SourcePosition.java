/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 2024 Michael G. Binz
 */
package de.michab.scream.util;

/**
 * Represents a source code position based on filename, line and column.
 *
 * @author micbinz
 */
public record SourcePosition( int line, int column, String filename )
{
    @Override
    public String toString()
    {
        return String.format( "%s (%d, %d)", filename, line, column );
    }
}
