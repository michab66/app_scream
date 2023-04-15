/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2022 Michael G. Binz
 */

//
// A scanner definition for Scheme r7rs 7.1.1 p61 
//

package de.michab.scream.frontend;

import de.michab.scream.RuntimeX;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.ScreamException.Code;

%%

/* Defines the name and the visibility modifiers for the generated class. */
%class SchemeScanner7
%public
%final

// Create a main function in the scanner class for testing purposes.
%debug

/* Defines the name, return type and thrown exceptions for the scanning
 * method. */
%function getNextToken
%type Token
%yylexthrow RuntimeX

/* Scanner should be able to scan unicode. */
%unicode
/* Enable line and column counting. */
%line
%column

%{
int commentNestCount = 0;
%}

%states NESTED_COMMENT

// r7rs
delimiter = whitespace |
  {vertical_line} |
  "(" |
  ")" |
  \" |
  ";"

// r7rs
intraline_whitespace = [ \t]

// r7rs
whitespace = {intraline_whitespace} | {line_ending}

// r7rs
vertical_line = \|

/* Matches end of line in a system independent and unicode compliant way.  This
 * expression is from the JFlex manual. */
line_ending = \r|\n|\r\n|\u2028|\u2029|\u000B|\u000C|\u0085

//
// Comments.
//

SINGLE_LINE_COMMENT = ";" .*
NC_BEGIN = "#|"
NC_END = "|#"
DATUM_COMMENT = "#;"

//
// Directives
//
directive = "#!fold-case" | "#!no-fold-case"

// r7rs
atmosphere = {whitespace} | {SINGLE_LINE_COMMENT} | {directive}

// r7rs
INTERTOKEN_SPACE = {atmosphere}+

// r7rs
IDENTIFIER = 
  {initial}({subsequent})* |
  {vertical_line} {symbol_element}* {vertical_line} |
  {peculiar_identifier}

// r7rs
initial = {letter}|{specialinitial}

// r7rs
letter = [a-zA-Z]

// r7rs
specialinitial = [\!\$\%\&\*\/\:\<\=\>\?\^\_\~]

// r7rs
subsequent = 
  {initial} | 
  {digit} |
  {special_subsequent}

// r7rs
digit =
  [0-9]
// r7rs - extended to support also uppercase digits.
hex_digit =
  {digit} | [a-fA-F]
// r7rs
explicit_sign = "+" |
  "-"
// r7rs
special_subsequent = 
  {explicit_sign} | 
  "." | 
  "@"

// r7rs
inline_hex_escape = "\x" {hex_scalar_value} \;
// r7rs
hex_scalar_value = {hex_digit}+

mnemonic_escape = \\a | \\b | \\t | \\n | \\r

// r7rs
peculiar_identifier =
  {explicit_sign} |
  {explicit_sign} {sign_subsequent} {subsequent}* |
  {explicit_sign} \. {dot_subsequent} {subsequent}* |
  \. {dot_subsequent} {subsequent}*

// r7rs
dot_subsequent = sign_subsequent |
  "."

// r7rs
sign_subsequent =
  {initial} |
  {explicit_sign} |
  "@"

DOT = "."

// r7rs
symbol_element = [^\|\\] |
  {inline_hex_escape} |
  {mnemonic_escape} |
  \\\|


BOOLEAN = \#t | \#f |\#true |\#false

//
// Characters
//
CHARACTER = \#\\ . 

CHARACTER_HEX = \#\\\x {hex_scalar_value}

// Character names are dynamically checked in the rule.
CHARACTER_NAME = \#\\ {letter}{letter}+ 

// TODO
anybutnewline = .

// r7rs
STRING = \"({stringelement})*\"

stringelement = [^\n\"] |
  {mnemonic_escape} |
  "\\\"" |
  "\\\\" |
  "\\" {intraline_whitespace}* {line_ending} {intraline_whitespace}* |
  {inline_hex_escape}

START_BYTEVECTOR = "#u8("

INTEGER = {sign}?{uinteger}
sign = [\+\-]
uinteger = ({digit})+
REAL = {sign}?{ureal}
ureal = {uinteger}?\.{uinteger}

STARTLIST = "("

STARTARRAY = "#("

END = ")"

QUOTE = \'
QUASIQUOTE = \`
UNQUOTE = \,
UNQUOTE_SPLICING = \,\@

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

  {CHARACTER} {
    // Get the matched token cutting off the '#\' part.
    String matched = yytext().substring( 2 );
    return new Token( matched.charAt( 0 ) );
  }

  {CHARACTER_HEX} {
    // Remove prefix '#\x'.
    String matched = yytext().substring( 3 );

    try 
    {
      var value = Integer.parseInt( matched, 16 );

      if ( value > 0 && value < 0xffff )
        return new Token( (char)value );
    }
    catch ( Exception e )
    {
    }
    
    throw RuntimeX.mScanUnexpectedCharacter( yyline+1, yycolumn+1, yytext() );
  }

  {CHARACTER_NAME} {
    // Remove prefix '#\'.
    String matched = yytext().substring( 2 );
    char result;

    switch ( matched )
    {
      case "alarm":
        return new Token( (char)0x07 );
      case "backspace":
        return new Token( (char)0x08 );
      case "delete":
        return new Token( (char)0x7f );
      case "escape":
        return new Token( (char)0x1b );
      case "newline":
        return new Token( (char)0x0a );
      case "null":
        return new Token( (char)0x00 );
      case "return":
        return new Token( (char)0x0d );
      case "space":
        return new Token( ' ' );
      case "tab":
        return new Token( '\t' );
      default:
        throw RuntimeX.mScanUnexpectedCharacter( yyline+1, yycolumn+1, yytext() );
    }
  }

  {START_BYTEVECTOR} {
    return new Token( Tk.Bytevector );
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

  {NC_BEGIN} {
    yybegin( NESTED_COMMENT );
    commentNestCount++;
  }
  
  {DATUM_COMMENT} {
      return Token.createToken( Tk.DatumComment );
  }
  
  /*
   * Error rules
   */

  // Catch unbalanced double quotes
  \"({stringelement})*{line_ending} {
    throw new FrontendX( yyline+1, yycolumn+1, Code.SCAN_UNBALANCED_QUOTE );
  }

  // Catch unmatched characters.
  . {
    throw new FrontendX( yyline+1, yycolumn+1, Code.SCAN_UNEXPECTED_CHAR, yytext() );
  }
  
  // Catch unmatched nested comments.  An NC_END token must never
  // be visible in YYINITIAL.
  {NC_END} {
      throw RuntimeX.mScanUnbalancedComment( yyline+1, yycolumn+1 );
  }
}

<NESTED_COMMENT> {

  {NC_BEGIN} {
    commentNestCount++;
  }

  {NC_END} {
    commentNestCount--;

    if ( commentNestCount == 0 )
      yybegin( YYINITIAL );
    else if ( commentNestCount > 0 )
      ;
    else
      throw RuntimeX.mScanUnbalancedComment( yyline+1, yycolumn+1 );
  }

  /* The first pattern consumes everything but the characters
   * '|' and '#' that appear in a nested comment start- or end-
   * token.
   * Pattern two and three consume a single of these characters,
   * respectively.
   *
   * This works since pattern matching is greedy.
   */
  ([^\|\#])+ | "#" | "|" {
    // Ignore.
  }
}

<<EOF>> {
  if ( commentNestCount > 0 )
      throw RuntimeX.mScanUnbalancedComment( yyline+1, yycolumn+1 );

  // If we reached EOF, we close the reader...
  yyreset( null );
  // ...before doing business as usual.
  return Token.createToken( Tk.Eof );
}
