/* $Id: Scheme.flex 8 2008-09-14 14:23:20Z binzm $
 *
 * Scream: frontend
 *
 * Released under Gnu Public License
 * Copyright (c) 1998-2001 Michael G. Binz
 */
package de.michab.scream.scanner;

%%

/* Defines the name and the visibility modifiers for the generated class. */
%class SchemeFlexScanner
%public
%final

/* Defines the name, return type and thrown exceptions for the scanning
 * method. */
%function getNextToken
%type Token
%yylexthrow FrontendX

/* Scanner should be able to scan unicode */
%unicode
/* Enable line and column counting */
%line
%column


/* Matches end of line in a system independent and unicode compliant way.  This
 * expression is from the JFlex manual. */
eol = \r|\n|\r\n|\u2028|\u2029|\u000B|\u000C|\u0085


/* The following is a direct translation of the lexical structure definition
 * given in the Scheme revised 5 standard.  Note that this is *not* complete,
 * the parts not used from Scream have been left out. */

/* Intertoken space is extended with form feed.  Finally it would be good
 * to support ::whitespace:: for full UNICODE compliance. */
INTERTOKEN_SPACE = [ \t\n\r\f]+

IDENTIFIER = {initial}({subsequent})* | {peculiaridentifier}
initial = {letter}|{specialinitial}
letter = [a-zA-Z]
specialinitial = [\!\$\%\&\*\/\:\<\=\>\?\^\_\~]
subsequent = {initial} | {digit} | {specialsubsequent}
digit = [0-9]
specialsubsequent = [\+\-\.\@]
peculiaridentifier = "\+" | "-" | "..."

DOT = "."

BOOLEAN = \#[tfTF]

CHAR = \#\\({charname}|{anybutnewline})
charname = "space"|"newline"
anybutnewline = .

STRING = \"({stringelement})*\"
stringelement = [^\n\"] | "\\\""

INTEGER = {sign}?{uinteger}
sign = [\+\-]
uinteger = ({digit})+
REAL = {sign}?{ureal}
ureal = {uinteger}?\.{uinteger}

STARTLIST = \(

STARTARRAY = \#\(

END = \)

QUOTE = \'
QUASIQUOTE = \`
UNQUOTE = \,
UNQUOTE_SPLICING = \,\@

/* In former times the line below read as 
 * "SINGLE_LINE_COMMENT = \;({anybutnewline})*{eol}".  This had the major
 * disadvantage of not matching a comment in the last line of a file that is
 * not terminated by a new line.  This version below uses the fact that
 * scanning is greedy and tries to match always the longest possible token.  
 * This means that always the whole line is matched, up to, but not including, 
 * the next newline or end of the file.  In the former case the newline is 
 * consumed as intertoken space, in the latter case the scanner terminates 
 * cleanly. */
SINGLE_LINE_COMMENT = \;({anybutnewline})*

%%

<YYINITIAL> {
  {IDENTIFIER} {
    return new Token( SchemeParser.TkSymbol, yytext() );
  }

  {DOT} {
    return Token.createToken( SchemeParser.TkDot );
  }

  {BOOLEAN} {
    return new Token( "#t".equalsIgnoreCase( yytext() ) );
  }

  {CHAR} {
    // Get the matched token cutting off the '#\' part.
    String matched = yytext().substring( 2 );
    char result;

    // In case the matched token is one of the named characters transform them
    // into their character representation...
    if ( matched.equals( "space" ) )
      result = ' ';
    else if ( matched.equals( "newline" ) )
      result = '\n';
    else
      // ...else just move the character.
      result = matched.charAt( 0 );

    return new Token( result );
  }

  {STRING} {
    // Remove the double quotes.
    String image = yytext();
    String noQuote = image.substring( 1, image.length()-1 );
    return new Token( SchemeParser.TkString, noQuote );
  }

  {INTEGER} {
    try
    {
      String longs = yytext();
      // Check if the matched text starts with an explicit plus sign ('+').
      // This cannot be handled by the Long.parsLong() call below, so the plus
      // has to be removed before.
      if ( longs.startsWith( "+" ) )
        longs = longs.substring( 1 );

      return new Token( Long.parseLong( longs ) );
    }
    catch ( NumberFormatException e )
    {
      throw new FrontendX( "INTERNAL_ERROR", new Object[]{ e.getMessage() } );
    }
  }

  {REAL} {
    try
    {
      return new Token( Double.valueOf( yytext() ).doubleValue() );
    }
    catch ( NumberFormatException e )
    {
      throw new FrontendX( "INTERNAL_ERROR", new Object[]{ e.getMessage() } );
    }
  }

  {STARTLIST} {
    return Token.createToken( SchemeParser.TkList );
  }

  {STARTARRAY} {
    return Token.createToken( SchemeParser.TkArray );
  }

  {END} {
    return Token.createToken( SchemeParser.TkEnd );
  }

  {QUOTE} {
    return Token.createToken( SchemeParser.TkQuote );
  }
  {QUASIQUOTE} {
    return Token.createToken( SchemeParser.TkQuasiQuote );
  }
  {UNQUOTE} {
    return Token.createToken( SchemeParser.TkUnquote );
  }
  {UNQUOTE_SPLICING} {
    return Token.createToken( SchemeParser.TkUnquoteSplicing );
  }

  {SINGLE_LINE_COMMENT} { /* ignore */ }

  {INTERTOKEN_SPACE} {/* ignore, signore */}

  <<EOF>> {
    // If we reached EOF, we close the reader...
    yyreset( null );
    // ...before doing business as usual.
    return Token.createToken( SchemeParser.TkEof );
  }

  /*
   * Error rules
   */

  // Catch unbalanced double quotes
  \"({stringelement})*{eol} {
    throw new FrontendX( yyline+1, yycolumn+1, "SCAN_UNBALANCED_QUOTE" );
  }

  // Catch unmatched characters.
  . {
    throw new FrontendX( yyline+1, yycolumn+1, "SCAN_UNEXPECTED_CHAR", yytext() );
  }
}
