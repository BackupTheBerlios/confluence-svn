(*
    InFormal Digital Logic Verification Environment
    Copyright (C) 2004 Tom Hawkins (tomahawkins@yahoo.com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
*)

{
(* Pre code. *)
}

(* Whitespace *)

let line_terminator = '\r' | '\n' | "\r\n"
let white_space     = line_terminator | [' ' '\t']

(* Comments *)

let comment_eol   = "//" [^'\n']* '\n'

(* Numbers *)

let dec_digit      = ['0' - '9']
(*
let bin_digit      = ['0' - '1']
let hex_digit      = ['0' - '9' 'A' - 'F' 'a' - 'f']
let integer        = dec_digit+ | dec_digit+ '\'' ['d' 'D'] dec_digit+ | dec_digit+ '\'' ['b' 'B'] bin_digit+ | dec_digit+ '\'' ['h' 'H'] hex_digit+
*)
let integer        = dec_digit+

(* Identifiers *)

let first_letter   = ['a' - 'z' 'A' - 'Z' '_']
let letter         = ['a' - 'z' 'A' - 'Z' '0' - '9' '_' '$']
let identifier     = first_letter letter*
let printable      = [^' ' '\n' '\r' '\t']
let identifier_esc = '\\' printable+

(* Strings *)

let string_char = [^'\n' '\r']
let string      = '"' string_char* '"'

rule token = parse 
  | "["                  { Psl_parser.Brackl              (Parser_util.next_lex lexbuf) }
  | "]"                  { Psl_parser.Brackr              (Parser_util.next_lex lexbuf) }
  | "("                  { Psl_parser.Parenl              (Parser_util.next_lex lexbuf) }
  | ")"                  { Psl_parser.Parenr              (Parser_util.next_lex lexbuf) }
  | "{"                  { Psl_parser.Bracel              (Parser_util.next_lex lexbuf) }
  | "}"                  { Psl_parser.Bracer              (Parser_util.next_lex lexbuf) }
  | ","                  { Psl_parser.Comma               (Parser_util.next_lex lexbuf) }
  | ";"                  { Psl_parser.Semicolon           (Parser_util.next_lex lexbuf) }
  | ":"                  { Psl_parser.Colon               (Parser_util.next_lex lexbuf) }
  | ".."                 { Psl_parser.Period_period       (Parser_util.next_lex lexbuf) }
  | "="                  { Psl_parser.Equal               (Parser_util.next_lex lexbuf) }
  | ":="                 { Psl_parser.Colon_equal         (Parser_util.next_lex lexbuf) }
  | "*"                  { Psl_parser.Aster               (Parser_util.next_lex lexbuf) }
  | "+"                  { Psl_parser.Plus                (Parser_util.next_lex lexbuf) }
  | "|->"                { Psl_parser.Bar_dash_gt         (Parser_util.next_lex lexbuf) }
  | "|=>"                { Psl_parser.Bar_equal_gt        (Parser_util.next_lex lexbuf) }
  | "<->"                { Psl_parser.Lt_dash_gt          (Parser_util.next_lex lexbuf) }
  | "->"                 { Psl_parser.Dash_gt             (Parser_util.next_lex lexbuf) }
  | "[*"                 { Psl_parser.Brackl_aster        (Parser_util.next_lex lexbuf) }
  | "[+]"                { Psl_parser.Brackl_plus_brackr  (Parser_util.next_lex lexbuf) }
  | "[->"                { Psl_parser.Brackl_dash_gt      (Parser_util.next_lex lexbuf) }
  | "[="                 { Psl_parser.Brackl_equal        (Parser_util.next_lex lexbuf) }
  | "&&"                 { Psl_parser.Amp_amp             (Parser_util.next_lex lexbuf) }
  | "&"                  { Psl_parser.Amp                 (Parser_util.next_lex lexbuf) }
  | "||"                 { Psl_parser.Bar_bar             (Parser_util.next_lex lexbuf) }
  | "|"                  { Psl_parser.Bar                 (Parser_util.next_lex lexbuf) }
  | "!"                  { Psl_parser.Bang                (Parser_util.next_lex lexbuf) }
  | "$"                  { Psl_parser.Dollar              (Parser_util.next_lex lexbuf) }
  | "@"                  { Psl_parser.At                  (Parser_util.next_lex lexbuf) }
  | "."                  { Psl_parser.Period              (Parser_util.next_lex lexbuf) }
  | "/"                  { Psl_parser.Slash               (Parser_util.next_lex lexbuf) }
  | "A"                  { Psl_parser.A                   (Parser_util.next_lex lexbuf) }
  | "AG"                 { Psl_parser.AG                  (Parser_util.next_lex lexbuf) }
  | "AF"                 { Psl_parser.AF                  (Parser_util.next_lex lexbuf) }
  | "AX"                 { Psl_parser.AX                  (Parser_util.next_lex lexbuf) }
  | "abort"              { Psl_parser.Abort               (Parser_util.next_lex lexbuf) }
  | "always"             { Psl_parser.Always              (Parser_util.next_lex lexbuf) }
  | "assert"             { Psl_parser.Assert              (Parser_util.next_lex lexbuf) }
  | "assume"             { Psl_parser.Assume              (Parser_util.next_lex lexbuf) }
  | "assume_guarantee"   { Psl_parser.Assume_guarantee    (Parser_util.next_lex lexbuf) }
  | "before"             { Psl_parser.Before              (Parser_util.next_lex lexbuf) }
  | "before!"            { Psl_parser.Before_bang         (Parser_util.next_lex lexbuf) }
  | "before!_"           { Psl_parser.Before_bang_        (Parser_util.next_lex lexbuf) }
  | "before_"            { Psl_parser.Before_             (Parser_util.next_lex lexbuf) }
  | "boolean"            { Psl_parser.Boolean             (Parser_util.next_lex lexbuf) }
  | "clock"              { Psl_parser.Clock               (Parser_util.next_lex lexbuf) }
  | "const"              { Psl_parser.Const               (Parser_util.next_lex lexbuf) }
  | "countones"          { Psl_parser.Countones           (Parser_util.next_lex lexbuf) }
  | "cover"              { Psl_parser.Cover               (Parser_util.next_lex lexbuf) }
  | "default"            { Psl_parser.Default             (Parser_util.next_lex lexbuf) }
  | "E"                  { Psl_parser.E                   (Parser_util.next_lex lexbuf) }
  | "EF"                 { Psl_parser.EF                  (Parser_util.next_lex lexbuf) }
  | "EG"                 { Psl_parser.EG                  (Parser_util.next_lex lexbuf) }
  | "EX"                 { Psl_parser.EX                  (Parser_util.next_lex lexbuf) }
  | "endpoint"           { Psl_parser.Endpoint            (Parser_util.next_lex lexbuf) }
  | "eventually!"        { Psl_parser.Eventually_bang     (Parser_util.next_lex lexbuf) }
  | "F"                  { Psl_parser.F                   (Parser_util.next_lex lexbuf) }
  | "fairness"           { Psl_parser.Fairness            (Parser_util.next_lex lexbuf) }
  | "fell"               { Psl_parser.Fell                (Parser_util.next_lex lexbuf) }
  | "forall"             { Psl_parser.Forall              (Parser_util.next_lex lexbuf) }
  | "G"                  { Psl_parser.G                   (Parser_util.next_lex lexbuf) }
  | "in"                 { Psl_parser.In                  (Parser_util.next_lex lexbuf) }
  | "inf"                { Psl_parser.Inf                 (Parser_util.next_lex lexbuf) }
  | "inherit"            { Psl_parser.Inherit             (Parser_util.next_lex lexbuf) }
  | "isunknown"          { Psl_parser.Isunknown           (Parser_util.next_lex lexbuf) }
  | "never"              { Psl_parser.Never               (Parser_util.next_lex lexbuf) }
  | "next"               { Psl_parser.Next                (Parser_util.next_lex lexbuf) }
  | "next!"              { Psl_parser.Next_bang           (Parser_util.next_lex lexbuf) }
  | "next_a"             { Psl_parser.Next_a              (Parser_util.next_lex lexbuf) }
  | "next_a!"            { Psl_parser.Next_a_bang         (Parser_util.next_lex lexbuf) }
  | "next_e"             { Psl_parser.Next_e              (Parser_util.next_lex lexbuf) }
  | "next_e!"            { Psl_parser.Next_e_bang         (Parser_util.next_lex lexbuf) }
  | "next_event"         { Psl_parser.Next_event          (Parser_util.next_lex lexbuf) }
  | "next_event!"        { Psl_parser.Next_event_bang     (Parser_util.next_lex lexbuf) }
  | "next_event_a"       { Psl_parser.Next_event_a        (Parser_util.next_lex lexbuf) }
  | "next_event_a!"      { Psl_parser.Next_event_a_bang   (Parser_util.next_lex lexbuf) }
  | "next_event_e"       { Psl_parser.Next_event_e        (Parser_util.next_lex lexbuf) }
  | "next_event_e!"      { Psl_parser.Next_event_e_bang   (Parser_util.next_lex lexbuf) }
  | "onehot"             { Psl_parser.Onehot              (Parser_util.next_lex lexbuf) }
  | "onehot0"            { Psl_parser.Onehot0             (Parser_util.next_lex lexbuf) }
  | "property"           { Psl_parser.Property            (Parser_util.next_lex lexbuf) }
  | "prev"               { Psl_parser.Prev                (Parser_util.next_lex lexbuf) }
  | "report"             { Psl_parser.Report              (Parser_util.next_lex lexbuf) }
  | "restrict"           { Psl_parser.Restrict            (Parser_util.next_lex lexbuf) }
  | "restrict_guarantee" { Psl_parser.Restrict_guarantee  (Parser_util.next_lex lexbuf) }
  | "rose"               { Psl_parser.Rose                (Parser_util.next_lex lexbuf) }
  | "sequence"           { Psl_parser.Sequence            (Parser_util.next_lex lexbuf) }
  | "stable"             { Psl_parser.Stable              (Parser_util.next_lex lexbuf) }
  | "strong"             { Psl_parser.Strong              (Parser_util.next_lex lexbuf) }
  | "U"                  { Psl_parser.U                   (Parser_util.next_lex lexbuf) }
  | "union"              { Psl_parser.Union               (Parser_util.next_lex lexbuf) }
  | "until"              { Psl_parser.Until               (Parser_util.next_lex lexbuf) }
  | "until!"             { Psl_parser.Until_bang          (Parser_util.next_lex lexbuf) }
  | "until!_"            { Psl_parser.Until_bang_         (Parser_util.next_lex lexbuf) }
  | "until_"             { Psl_parser.Until_              (Parser_util.next_lex lexbuf) }
  | "vmode"              { Psl_parser.Vmode               (Parser_util.next_lex lexbuf) }
  | "vprop"              { Psl_parser.Vprop               (Parser_util.next_lex lexbuf) }
  | "vunit"              { Psl_parser.Vunit               (Parser_util.next_lex lexbuf) }
  | "within"             { Psl_parser.Within              (Parser_util.next_lex lexbuf) }
  | "X"                  { Psl_parser.X                   (Parser_util.next_lex lexbuf) }
  | "X!"                 { Psl_parser.X_bang              (Parser_util.next_lex lexbuf) }

  | "posedge"            { Psl_parser.Posedge             (Parser_util.next_lex lexbuf) }
  | "negedge"            { Psl_parser.Negedge             (Parser_util.next_lex lexbuf) }

  | integer              { Psl_parser.Integer             (Parser_util.next_lex lexbuf) }
  | identifier           { Psl_parser.Identifier          (Parser_util.next_lex lexbuf) }
  | identifier_esc       { Psl_parser.Identifier_esc      (Parser_util.next_lex lexbuf) }

  | string               { Psl_parser.String              (Parser_util.next_lex lexbuf) }

  | eof                  { Psl_parser.EOF                 (Parser_util.next_lex lexbuf) }

  | white_space          { let _ = Parser_util.next_lex lexbuf in token lexbuf }

  | comment_eol          { let _ = Parser_util.next_lex lexbuf in token lexbuf }
  | "/*"                 { let _ = Parser_util.next_lex lexbuf in comment lexbuf }

  | _                    { let token = Parser_util.next_lex lexbuf in
                           Parser_util.error ("Unrecognized character: '" ^ (fst token) ^ "'");
                           Psl_parser.Lexer_error token }

and comment = parse
  | "*/"                 { let _ = Parser_util.next_lex lexbuf in token lexbuf }

  | eof                  { let token = Parser_util.next_lex lexbuf in
                           Parser_util.error ("End of File without closing comment.");
                           Psl_parser.Lexer_error token }

  | _                    { let _ = Parser_util.next_lex lexbuf in comment lexbuf }

{
(* Post code. *)
}

