/*
 * Scream @ https://github.com/urschleim/scream
 *
 * Copyright © 1998-2023 Michael G. Binz
 */

//
// A scanner definition for Scheme r7rs 7.1.1 p61 
//

package de.michab.scream.frontend;

import de.michab.scream.RuntimeX;
import de.michab.scream.frontend.Token.Tk;
import de.michab.scream.RuntimeX.Code;

%%

/* Defines the name and the visibility modifiers for the generated class. */
%class SchemeScanner7
%public
%final

// Create a main function in the scanner class for testing purposes.
// %debug

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

dquote = "\""

intraline_whitespace =
  [ \t]

whitespace =
  {intraline_whitespace} |
  {line_ending}

vertical_line =
  "|"

/* Matches end of line in a system independent and unicode compliant way.  This
 * expression is from the JFlex manual. */
line_ending = \r|\n|\r\n|\u2028|\u2029|\u000B|\u000C|\u0085

//
// Comments.
//

SINGLE_LINE_COMMENT =
  ";" .*
  
NC_BEGIN =
  "#|"

NC_END =
  "|#"

DATUM_COMMENT =
  "#;"

//
// Directives
//
directive =
  "#!fold-case" |
  "#!no-fold-case"

atmosphere = 
  {whitespace} |
  {SINGLE_LINE_COMMENT} |
  {directive}

INTERTOKEN_SPACE =
  {atmosphere}+

IDENTIFIER = 
  {initial}({subsequent})* |
  {vertical_line} {symbol_element}* {vertical_line} |
  {peculiar_identifier}

initial =
  {letter} |
  {specialinitial}

letter =
  [a-zA-Z]

specialinitial =
  [\!\$\%\&\*\/\:\<\=\>\?\^\_\~]

subsequent = 
  {initial} | 
  {digit} |
  {special_subsequent}

digit =
  [0-9]

// r7rs - extended to support also uppercase digits.
hex_digit =
  {digit} |
  [a-fA-F]

special_subsequent = 
  {sign} | 
  "." | 
  "@"

inline_hex_escape =
  "\x" {hex_scalar_value} \;

hex_scalar_value =
  {hex_digit}+

mnemonic_escape = \\a |
  \\b |
  \\t |
  \\n |
  \\r

peculiar_identifier =
  {sign} |
  {sign} {sign_subsequent} {subsequent}* |
  {sign} \. {dot_subsequent} {subsequent}* |
  \. {dot_subsequent} {subsequent}*

dot_subsequent = sign_subsequent |
  "."

sign_subsequent =
  {initial} |
  {sign} |
  "@"

DOT = "."

symbol_element =
  [^\|\\] |
  {inline_hex_escape} |
  {mnemonic_escape} |
  \\\|


BOOLEAN =
  "#t" | "#f" | "#true" | "#false" | 
  "#T" | "#F" | "#TRUE" | "#FALSE"

//
// Characters
//

char_prefix =
  "#\\"

CHARACTER =
  {char_prefix} . 

CHARACTER_HEX =
  {char_prefix} "x" {hex_scalar_value}

// Character names are dynamically checked in the rule.
CHARACTER_NAME =
  {char_prefix} {letter}{letter}+ 

//
// Strings
//
STRING =
  {dquote} ({stringelement})* {dquote}

stringelement =
  [^\n\"] |
  {mnemonic_escape} |
  "\\\"" |
  "\\\\" |
  "\\" {intraline_whitespace}* {line_ending} {intraline_whitespace}* |
  {inline_hex_escape}

//
// Bytevector.
//
START_BYTEVECTOR =
  "#u8("

//
// Numbers.
//

exponent_marker = "e"
sign = "+" | "-"
exactness = "#i" | "#e"
radix_2 = "#b"
radix_8 = "#o"
radix_10 = "#d"
radix_16 = "#x"
digit_2  = "0" | "1"
digit_8  = {digit_2}  | "2" | "3" | "4" | "5" | "6" | "7"
digit_10 = {digit_8}  | "8" | "9"
digit_16 = {digit_10} | [aA] | [bB] | [cC] | [dD] | [eE] | [fF]

suffix = {exponent_marker} {sign}? {digit_10}+

prefix_2 =
  {radix_2} {exactness}? |
  {exactness}? {radix_2}
prefix_8 =
  {radix_8} {exactness}? |
  {exactness}? {radix_8}
prefix_10 =
  {radix_10} {exactness}? |
  {exactness}? {radix_10} |
  {exactness}
prefix_16 =
  {radix_16} {exactness}? |
  {exactness}? {radix_16}

uinteger_2 =
  {digit_2}+
uinteger_8 =
  {digit_8}+
uinteger_10 =
  {digit_10}+
uinteger_16 =
  {digit_16}+

integer_2 =
  {prefix_2} {sign}? {uinteger_2}
integer_8 =
  {prefix_8} {sign}?{uinteger_8}
integer_10 =
  {prefix_10}? {sign}? {uinteger_10}
integer_16 =
  {prefix_16} {sign}? {uinteger_16}

INTEGER =
  {integer_2} |
  {integer_8} |
  {integer_10} |
  {integer_16}

decimal_10 =
  {uinteger_10} {suffix} |
  {sign}? {uinteger_10}? \. {uinteger_10} {suffix}? |
  {sign}? {uinteger_10} \. {uinteger_10}? {suffix}?

REAL =
  {exactness}? {decimal_10}

STARTLIST =
  "("

STARTARRAY =
  "#("

END =
  ")"

QUOTE =
  "'"

QUASIQUOTE =
  "`"

UNQUOTE =
  ","

UNQUOTE_SPLICING =
  ",@"

LABEL = "#" {uinteger_10} "#"
LABEL_REFERENCE = "#" {uinteger_10} "="

%%

<YYINITIAL> {
  {IDENTIFIER} {
    return new Token( Tk.Symbol, yytext() );
  }

  {DOT} {
    return Token.createToken( Tk.Dot );
  }

  {BOOLEAN} {
    return new Token(
     yytext().toLowerCase().startsWith( "#t" ) );
  }

  {CHARACTER} {
    return new Token( yycharat( 2 ) );
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

  {LABEL} {
    return new Token( Tk.Label );
  }
  
  {LABEL_REFERENCE} {
    return new Token( Tk.LabelReference );
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
    var matched = yytext();
    
    boolean inexact = matched.contains( "#i" );
    boolean exact = matched.contains( "#e" );

    if ( inexact )
        matched = matched.replace( "#i", "" );
    if ( exact )
        matched = matched.replace( "#e", "" );

    if ( inexact && exact )
        throw new InternalError( "inexact && exact" );
        
    boolean r2 = matched.contains( "#b" );
    boolean r8 = matched.contains( "#o" );
    boolean r10 = matched.contains( "#d" );
    boolean r16 = matched.contains( "#x" );

    int radix = 10;

    if ( r2 )
    {
        matched = matched.replace( "#b", "" );
        radix = 2;
    }
    else if ( r8 ) 
    {
        matched = matched.replace( "#o", "" );
        radix = 8;
    }
    else if ( r10 )
    {
        matched = matched.replace( "#d", "" );
        radix = 10;
    }
    else if ( r16 )
    {
        matched = matched.replace( "#x", "" );
        radix = 16;
    }

    try
    {
      return new Token( Long.parseLong( matched, radix ) );
    }
    catch ( NumberFormatException e )
    {
      throw new FrontendX( Code.INTERNAL_ERROR, e.getMessage() );
    }
  }

  {REAL} {
    var matched = yytext();
    
    boolean inexact = matched.contains( "#i" );
    boolean exact = matched.contains( "#e" );

    if ( inexact )
        matched = matched.replace( "#i", "" );
    if ( exact )
        matched = matched.replace( "#e", "" );

    if ( inexact && exact )
        throw new InternalError( "inexact && exact" );

    try
    {
      return new Token( Double.valueOf( matched ).doubleValue() );
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
