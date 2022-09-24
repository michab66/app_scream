/* $Id: Token.java 1 2008-09-19 16:30:02Z binzm $
 *
 * Scream: Frontend
 *
 * Copyright (c) 1998,2022 Michael G. Binz
 */

package de.michab.scream.frontend;

/**
 * The Scheme tokens used by the frontend.
 * @author micbinz
 */
enum Tk {
    Symbol,
    Integer,
    Double,
    Array,
    List,
    // TODO check if used in scanner. Seems not.
    End,
    String,
    Quote,
    Dot,
    Boolean,
    Char,
    Eof,
    QuasiQuote,
    Unquote,
    UnquoteSplicing
}
