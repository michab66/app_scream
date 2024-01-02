/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2024 Michael G. Binz
 */
package de.michab.scream.frontend;

import de.michab.scream.RuntimeX;
import de.michab.scream.util.SourcePosition;

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
     * @param sp The current source code position.
     * @param yytext The offending part of the source file.
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     */
    public FrontendX( SourcePosition sp, Code key, String yytext )
    {
        super( key, new Object[]{ "" + sp.line(), "" + sp.column(), yytext } );
    }

    /**
     * Creates a frontend exception.  The {@code line} and
     * {@code column} parameters are internally appended to the argument
     * list used in exception resolution and message generation.
     *
     * @param sp The current source code position.
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     */
    public FrontendX( SourcePosition sp, Code key )
    {
        super( key, new Object[]{ "" + sp.line(), "" + sp.column() } );
    }

    /**
     * Creates a frontend exception.  The passed argument list is used in
     * exception resolution and message generation.
     *
     * @param sp The current source code position.
     * @param key  The message key.  See
     *             {@link de.michab.scream.ScreamException ScreamException}
     *             for further explanation of message key resolution.
     * @param args The exception's arguments.
     */
    public FrontendX( SourcePosition sp, Code key, Object ... args )
    {
        super( key, args );
    }

    private static final long serialVersionUID = -1102603164031310511L;
}
