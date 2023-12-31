/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */
package de.michab.scream.frontend;

import de.michab.scream.RuntimeX;

/**
 * A exception common for all frontend related errors thrown by the scanning
 * and parsing phases.  For a description of the {@code key} argument
 * used by all of the constructors see
 * {@link de.michab.scream.ScreamException ScreamException}.
 */
public class FrontendX
    extends RuntimeX
{
    /**
     * Creates a frontend exception.  The {@code line}, {@code column}
     * and {@code yytext} parameters are internally added to the argument
     * list used in exception resolution and message generation.
     *
     * @param line The error's line number.  Line counting begins with 1.
     * @param column The error's column number.  Column counting begins with 1.
     * @param yytext The offending part of the source file.
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     */
    public FrontendX( int line, int column, String filename, Code key, String yytext )
    {
        super( key, new Object[]{ "" + line, "" + column, yytext } );
    }

    /**
     * Creates a frontend exception.  The {@code line} and
     * {@code column} parameters are internally added to the argument
     * list used in exception resolution and message generation.
     *
     * @param line The error's line number.  Line counting begins with 1.
     * @param column The error's column number.  Column counting begins with 1.
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     */
    public FrontendX( int line, int column, String filename, Code key )
    {
        super( key, new Object[]{ "" + line, "" + column } );
    }

    /**
     * Creates a frontend exception.  The passed argument list is used in
     * exception resolution and message generation.
     *
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     * @param args The exception's arguments.
     */
    public FrontendX( int line, int column, String filename, Code key, Object ... args )
    {
        super( key, args );
    }

    private static final long serialVersionUID = -1102603164031310511L;
}
