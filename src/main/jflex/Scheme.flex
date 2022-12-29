/*
 * Scream @ https://github.com/michab/dev_smack
 *
 * Copyright © 1998-2022 Michael G. Binz
 */
package de.michab.scream.frontend;

import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.ScreamException.Code;

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

/* Scanner should be able to scan unicode. */
%unicode
/* Enable line and column counting. */
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
    return new Token( Tk.Symbol, yytext() );
  }

  {DOT} {
    return Token.createToken( Tk.Dot );
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
    return new Token( Tk.String, noQuote );
  }

  {INTEGER} {
    try
    {
      return new Token( Long.parseLong( yytext() ) );
    }
    catch ( NumberFormatException e )
    {
      throw new FrontendX( Code.INTERNAL_ERROR, e.getMessage() );
    }
  }

  {REAL} {
    try
    {
      return new Token( Double.valueOf( yytext() ).doubleValue() );
    }
    catch ( NumberFormatException e )
    {
      throw new FrontendX( Code.INTERNAL_ERROR, e.getMessage() );
    }
  }

  {STARTLIST} {
    return Token.createToken( Tk.List );
  }

  {STARTARRAY} {
    return Token.createToken( Tk.Array );
  }

  {END} {
    return Token.createToken( Tk.End );
  }

  {QUOTE} {
    return Token.createToken( Tk.Quote );
  }
  {QUASIQUOTE} {
    return Token.createToken( Tk.QuasiQuote );
  }
  {UNQUOTE} {
    return Token.createToken( Tk.Unquote );
  }
  {UNQUOTE_SPLICING} {
    return Token.createToken( Tk.UnquoteSplicing );
  }

  {SINGLE_LINE_COMMENT} { /* ignore */ }

  {INTERTOKEN_SPACE} {/* ignore */}

  <<EOF>> {
    // If we reached EOF, we close the reader...
    yyreset( null );
    // ...before doing business as usual.
    return Token.createToken( Tk.Eof );
  }

  /*
   * Error rules
   */

  // Catch unbalanced double quotes
  \"({stringelement})*{eol} {
    throw new FrontendX( yyline+1, yycolumn+1, Code.SCAN_UNBALANCED_QUOTE );
  }

  // Catch unmatched characters.
  . {
    throw new FrontendX( yyline+1, yycolumn+1, Code.SCAN_UNEXPECTED_CHAR, yytext() );
  }
}
