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
%class SchemeComments7
%public
%final

// Create a main function in the scanner class for testing purposes.
%debug

/* Defines the name, return type and thrown exceptions for the scanning
 * method. */
%function getNextToken
%type String
%yylexthrow Exception

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
intraline_whitespace = [ \t]

// r7rs
whitespace = {intraline_whitespace} | {line_ending}

/* Matches end of line in a system independent and unicode compliant way.  This
 * expression is from the JFlex manual. */
// r7rs
line_ending = \r|\n|\r\n|\u2028|\u2029|\u000B|\u000C|\u0085

// r7rs -- TODO
comment = \;.*

NC_BEGIN = "#|"
NC_END = "|#"

// r7rs
atmosphere = {whitespace} | {comment}

// r7rs
INTERTOKEN_SPACE = {atmosphere}+


digit =
  [0-9]

ANY = ([^\|\#])+

INTEGER = ({digit})+

%%

<YYINITIAL> {

  {INTERTOKEN_SPACE} {/* ignore */}

  {INTEGER} {
    return yytext();
  
  }
  
  {NC_BEGIN} {
    System.err.println( yytext() );
    yybegin( NESTED_COMMENT );
    commentNestCount++;
  }

  {NC_END} {
    throw new InternalError( "Unmatched comment end." );
  }
}

<NESTED_COMMENT> {

  {NC_BEGIN} {
    System.err.println( "NESTED_COMMENT:NC_BEGIN" + yytext() );

    commentNestCount++;
    // Consume.
  }

  {NC_END} {
    System.err.println( "NESTED_COMMENT:NC_END" + yytext() );

    commentNestCount--;

    if ( commentNestCount == 0 )
      yybegin( YYINITIAL );
    else if ( commentNestCount > 0 )
      ;
    else
      throw new InternalError( "Unmatched comment end." );
  }

  ([^\|\#])+ | "#" | "|" {
    System.err.println( "NESTED_COMMENT:ANY" + yytext() );
    // Ignore.
  }
}

<<EOF>> {
  if ( commentNestCount > 0 )
    throw new InternalError( "Unterminated comment." );

  // If we reached EOF, we close the reader...
  yyreset( null );
  // ...before doing business as usual.
  return "Tk.Eof";
}
