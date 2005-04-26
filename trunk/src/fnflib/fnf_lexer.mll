(*
    FNF: Free Netlist Format
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

let white_space     = [' ' '\t' '\r' '\n']

(* Comments *)

let comment = ';' [^ '\n']* '\n'

(* Integers *)

let dec_digit      = ['0' - '9']
let integer        = dec_digit+

(* Strings *)

let string_char = [^'"' '\\'] | ('\\' _)
let string      = '"' string_char* '"'

rule token = parse 
  | "("                  { Fnf_parser.Parenl              (Parser_util.next_lex lexbuf) }
  | ")"                  { Fnf_parser.Parenr              (Parser_util.next_lex lexbuf) }
  | "scope"              { Fnf_parser.Scope               (Parser_util.next_lex lexbuf) }
  | "input"              { Fnf_parser.Input               (Parser_util.next_lex lexbuf) }
  | "output"             { Fnf_parser.Output              (Parser_util.next_lex lexbuf) }
  | "name"               { Fnf_parser.Name                (Parser_util.next_lex lexbuf) }
  | "dangle"             { Fnf_parser.Dangle              (Parser_util.next_lex lexbuf) }
  | "const"              { Fnf_parser.Const               (Parser_util.next_lex lexbuf) }
  | "buf"                { Fnf_parser.Buf                 (Parser_util.next_lex lexbuf) }
  | "not"                { Fnf_parser.Not                 (Parser_util.next_lex lexbuf) }
  | "and"                { Fnf_parser.And                 (Parser_util.next_lex lexbuf) }
  | "xor"                { Fnf_parser.Xor                 (Parser_util.next_lex lexbuf) }
  | "or"                 { Fnf_parser.Or                  (Parser_util.next_lex lexbuf) }
  | "concat"             { Fnf_parser.Concat              (Parser_util.next_lex lexbuf) }
  | "select"             { Fnf_parser.Select              (Parser_util.next_lex lexbuf) }
  | "eq"                 { Fnf_parser.Eq                  (Parser_util.next_lex lexbuf) }
  | "lt"                 { Fnf_parser.Lt                  (Parser_util.next_lex lexbuf) }
  | "add"                { Fnf_parser.Add                 (Parser_util.next_lex lexbuf) }
  | "sub"                { Fnf_parser.Sub                 (Parser_util.next_lex lexbuf) }
  | "mul"                { Fnf_parser.Mul                 (Parser_util.next_lex lexbuf) }
  | "mux"                { Fnf_parser.Mux                 (Parser_util.next_lex lexbuf) }
  | "ff"                 { Fnf_parser.Ff                  (Parser_util.next_lex lexbuf) }
  | "ffc"                { Fnf_parser.Ffc                 (Parser_util.next_lex lexbuf) }
  | "bbox"               { Fnf_parser.Bbox                (Parser_util.next_lex lexbuf) }

  | integer              { Fnf_parser.Integer             (Parser_util.next_lex lexbuf) }

  | string               { Fnf_parser.String              (Parser_util.next_lex lexbuf) }

  | eof                  { Fnf_parser.EOF                 (Parser_util.next_lex lexbuf) }

  | comment              { let _ = Parser_util.next_lex lexbuf in token lexbuf }

  | white_space          { let _ = Parser_util.next_lex lexbuf in token lexbuf }

  | _                    { let token = Parser_util.next_lex lexbuf in
                           Parser_util.error ("Unrecognized character: '" ^ (fst token) ^ "'");
                           Fnf_parser.Lexer_error token }

{
(* Post code. *)
}

