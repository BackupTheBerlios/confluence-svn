(*
    Confluence System Design Language Compiler
    Copyright (C) 2003-2004 Tom Hawkins (tomahawkins@yahoo.com)

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
(* Pre Code *)
exception Error of string;;

let commentDepth = ref 0;;

let buildInteger (loc, s) =
  CfParser.INTEGER (loc, Intbig.intbig_of_string s);;

let buildChar (loc, s) =
  CfParser.INTEGER (loc, Intbig.intbig_of_char s.[1]);;

let buildFloat (loc, s) =
  CfParser.FLOAT (loc, float_of_string s);;

let buildBoolean (loc, s) =
  CfParser.BOOLEAN (loc, s = "true");;

let hexCharToBits hex =
  match hex with
    '0'       -> "0000"
  | '1'       -> "0001"
  | '2'       -> "0010"
  | '3'       -> "0011"
  | '4'       -> "0100"
  | '5'       -> "0101"
  | '6'       -> "0110"
  | '7'       -> "0111"
  | '8'       -> "1000"
  | '9'       -> "1001"
  | 'A' | 'a' -> "1010"
  | 'B' | 'b' -> "1011"
  | 'C' | 'c' -> "1100"
  | 'D' | 'd' -> "1101"
  | 'E' | 'e' -> "1110"
  | 'F' | 'f' -> "1111"
  | _ -> raise (Error "Lex Error: Invalid hex character.");;

let rec hexToBits hex =
  if hex = "" then "" else
    hexCharToBits (String2.left_char hex) ^ hexToBits (String2.right_string hex);;

let buildVector (loc, s) =
  let tmp = CfParser.VECTOR (loc, String.sub s 1 (String.length s - 2)) in
  if String.length s >= 3 then
    match String2.take_left s 3 with
      "'0x" | "'1x" -> CfParser.VECTOR (loc, hexToBits (String.sub s 3 (String.length s - 4)))
    | "'0b" | "'1b" -> CfParser.VECTOR (loc, String.sub s 3 (String.length s - 4))
    | _ -> tmp
  else
    tmp;;

}

(* Whitespace *)

let lineTerminator = '\r' | '\n' | "\r\n"
let inputCharacter = [^ '\r' '\n']
let whiteSpace     = lineTerminator | [' ' '\t']

(* Other stuff. *)

let escape      = ['\\' '"' 'n' 't' 'r']
let stringChar  = [^ '\\' '"']
let pseudoChar  = '\\' escape
let string      = '"' (stringChar | pseudoChar)* '"'

let dec         = ['0' - '9']
let hex         = ['0' - '9' 'A' - 'F' 'a' - 'f']
let bin         = ['0' '1']

let int         = '-'? dec+ | "0x" hex+ | "1x" hex+ | "0b" bin+ | "1b" bin+

let vector      = '\'' (bin* | ("0x" hex* | "1x" hex*) | ("0b" bin* | "1b" bin*)) '\''

let float       = '-'? dec+ '.' dec+ ('e' '-'? dec+ )?

let letter      = ['A' - 'Z' 'a' - 'z' '_' '0' - '9']
let firstLetter = ['A' - 'Z' 'a' - 'z']
let name        = firstLetter letter*
let char        = '@' [^ ' ' '\r' '\n' '\t']
let identifier = 
    name
  | "(')"
  | "(!)"
  | "(~)"
  | "(head)"
  | "(tail)"
  | "(length)"
  | "(width)"
  | "('~')"
  | "('msb')"
  | "('msbs')"
  | "('lsb')"
  | "('lsbs')"
  | "(`!`)"
  | "(`X`)"
  | "(**)"
  | "('**')"
  | "(*)"
  | "(/)"
  | "(%)"
  | "('*')"
  | "('/')"
  | "('%')"
  | "('*+')"
  | "(+)"
  | "(-)"
  | "('+')"
  | "('-')"
  | "(<<)"
  | "(>>)"
  | "('<<')"
  | "('>>')"
  | "('>>+')"
  | "(<)"
  | "(>)"
  | "(<=)"
  | "(>=)"
  | "('<')"
  | "('>')"
  | "('<=')"
  | "('>=')"
  | "('<+')"
  | "('>+')"
  | "('<=+')"
  | "('>=+')"
  | "(#)"
  | "('#')"
  | "(::)"
  | "(++)"
  | "('++')"
  | "(==)"
  | "(!=)"
  | "('==')"
  | "('!=')"
  | "(&)"
  | "('&')"
  | "(^)"
  | "('^')"
  | "(|)"
  | "('|')"
  | "(&&)"
  | "(||)"
  | "(`&&`)"
  | "(`^^`)"
  | "(`||`)"
  | "(`<->`)"
  | "(`->`)"
  | "(`U`)"
  | "(:)"
  | "('then')"

rule token = parse 
  | int           { buildInteger (CfParserUtil.next_lex lexbuf) }
  | char          { buildChar    (CfParserUtil.next_lex lexbuf) }
  | float         { buildFloat   (CfParserUtil.next_lex lexbuf) }
  | string        { CfParser.STRING                   (CfParserUtil.next_lex lexbuf) }
  | vector        { buildVector  (CfParserUtil.next_lex lexbuf) }
  | "true"        { buildBoolean (CfParserUtil.next_lex lexbuf) }
  | "false"       { buildBoolean (CfParserUtil.next_lex lexbuf) }
  | "_"           { CfParser.FREE                     (CfParserUtil.next_lex lexbuf) }
  | "end"         { CfParser.END                      (CfParserUtil.next_lex lexbuf) }
  | "component"   { CfParser.COMPONENT                (CfParserUtil.next_lex lexbuf) }
  | "comp"        { CfParser.COMP                     (CfParserUtil.next_lex lexbuf) }
  | "prim"        { CfParser.PRIM                     (CfParserUtil.next_lex lexbuf) }
  | "if"          { CfParser.IF                       (CfParserUtil.next_lex lexbuf) }
  | "ef"          { CfParser.EF                       (CfParserUtil.next_lex lexbuf) }
  | "else"        { CfParser.ELSE                     (CfParserUtil.next_lex lexbuf) }
  | "with"        { CfParser.WITH                     (CfParserUtil.next_lex lexbuf) }
  | "is"          { CfParser.IS                       (CfParserUtil.next_lex lexbuf) }
  | "local"       { CfParser.LOCAL                    (CfParserUtil.next_lex lexbuf) }
  | "import"      { CfParser.IMPORT                   (CfParserUtil.next_lex lexbuf) }
  | "environment" { CfParser.ENVIRONMENT              (CfParserUtil.next_lex lexbuf) }
  | "rootenvironment" { CfParser.ROOTENVIRONMENT      (CfParserUtil.next_lex lexbuf) }
  | "fileloc"     { let (l, _) = CfParserUtil.next_lex lexbuf in CfParser.STRING (l, "\"" ^ Loc.toString l ^ "\"") }
  | "{"           { CfParser.BRACE_LEFT               (CfParserUtil.next_lex lexbuf) }
  | "}"           { CfParser.BRACE_RIGHT              (CfParserUtil.next_lex lexbuf) }
  | "("           { CfParser.PAREN_LEFT               (CfParserUtil.next_lex lexbuf) }
  | ")"           { CfParser.PAREN_RIGHT              (CfParserUtil.next_lex lexbuf) }
  | "["           { CfParser.BRACKET_LEFT             (CfParserUtil.next_lex lexbuf) }
  | "]"           { CfParser.BRACKET_RIGHT            (CfParserUtil.next_lex lexbuf) }
  | "$"           { CfParser.DOLLAR                   (CfParserUtil.next_lex lexbuf) }
  | ":"           { CfParser.COLON                    (CfParserUtil.next_lex lexbuf) }
  | eof           { CfParser.EOF                      (CfParserUtil.next_lex lexbuf) }
  | "(*"          { let _ = CfParserUtil.next_lex lexbuf in incr commentDepth; comment lexbuf; token lexbuf }
  | whiteSpace    { let _ = CfParserUtil.next_lex lexbuf in token lexbuf }

  | "."       { CfParser.OP_DOT                       (CfParserUtil.next_lex lexbuf) }
  | "'"       { CfParser.OP_VEC_SELECT                (CfParserUtil.next_lex lexbuf) }
  | "!"       { CfParser.OP_NOT                       (CfParserUtil.next_lex lexbuf) }
  | "~"       { CfParser.OP_BW_NOT                    (CfParserUtil.next_lex lexbuf) }
  | "head"    { CfParser.OP_HEAD                      (CfParserUtil.next_lex lexbuf) }
  | "tail"    { CfParser.OP_TAIL                      (CfParserUtil.next_lex lexbuf) }
  | "length"  { CfParser.OP_LENGTH                    (CfParserUtil.next_lex lexbuf) }
  | "width"   { CfParser.OP_WIDTH                     (CfParserUtil.next_lex lexbuf) }
  | "'~'"     { CfParser.OP_VEC_NOT                   (CfParserUtil.next_lex lexbuf) }
  | "'msb'"   { CfParser.OP_VEC_MSB                   (CfParserUtil.next_lex lexbuf) }
  | "'msbs'"  { CfParser.OP_VEC_MSBS                  (CfParserUtil.next_lex lexbuf) }
  | "'lsb'"   { CfParser.OP_VEC_LSB                   (CfParserUtil.next_lex lexbuf) }
  | "'lsbs'"  { CfParser.OP_VEC_LSBS                  (CfParserUtil.next_lex lexbuf) }
  | "`!`"     { CfParser.OP_PROP_NOT                  (CfParserUtil.next_lex lexbuf) }
  | "`X`"     { CfParser.OP_PROP_X                    (CfParserUtil.next_lex lexbuf) }
  | "**"      { CfParser.OP_POW                       (CfParserUtil.next_lex lexbuf) }
  | "'**'"    { CfParser.OP_VEC_POW                   (CfParserUtil.next_lex lexbuf) }
  | "*"       { CfParser.OP_MUL                       (CfParserUtil.next_lex lexbuf) }
  | "/"       { CfParser.OP_DIV                       (CfParserUtil.next_lex lexbuf) }
  | "%"       { CfParser.OP_MOD                       (CfParserUtil.next_lex lexbuf) }
  | "'*'"     { CfParser.OP_VEC_MUL                   (CfParserUtil.next_lex lexbuf) }
  | "'/'"     { CfParser.OP_VEC_DIV                   (CfParserUtil.next_lex lexbuf) }
  | "'%'"     { CfParser.OP_VEC_MOD                   (CfParserUtil.next_lex lexbuf) }
  | "'*+'"    { CfParser.OP_VEC_MUL_S                 (CfParserUtil.next_lex lexbuf) }
  | "+"       { CfParser.OP_ADD                       (CfParserUtil.next_lex lexbuf) }
  | "-"       { CfParser.OP_SUB                       (CfParserUtil.next_lex lexbuf) }
  | "'+'"     { CfParser.OP_VEC_ADD                   (CfParserUtil.next_lex lexbuf) }
  | "'-'"     { CfParser.OP_VEC_SUB                   (CfParserUtil.next_lex lexbuf) }
  | "<<"      { CfParser.OP_LSHIFT                    (CfParserUtil.next_lex lexbuf) }
  | ">>"      { CfParser.OP_RSHIFT                    (CfParserUtil.next_lex lexbuf) }
  | "'<<'"    { CfParser.OP_VEC_LSHIFT                (CfParserUtil.next_lex lexbuf) }
  | "'>>'"    { CfParser.OP_VEC_RSHIFT                (CfParserUtil.next_lex lexbuf) }
  | "'>>+'"   { CfParser.OP_VEC_RSHIFT_S              (CfParserUtil.next_lex lexbuf) }
  | "<"       { CfParser.OP_LT                        (CfParserUtil.next_lex lexbuf) }
  | ">"       { CfParser.OP_GT                        (CfParserUtil.next_lex lexbuf) }
  | "<="      { CfParser.OP_LE                        (CfParserUtil.next_lex lexbuf) }
  | ">="      { CfParser.OP_GE                        (CfParserUtil.next_lex lexbuf) }
  | "'<'"     { CfParser.OP_VEC_LT                    (CfParserUtil.next_lex lexbuf) }
  | "'>'"     { CfParser.OP_VEC_GT                    (CfParserUtil.next_lex lexbuf) }
  | "'<='"    { CfParser.OP_VEC_LE                    (CfParserUtil.next_lex lexbuf) }
  | "'>='"    { CfParser.OP_VEC_GE                    (CfParserUtil.next_lex lexbuf) }
  | "'<+'"    { CfParser.OP_VEC_LT_S                  (CfParserUtil.next_lex lexbuf) }
  | "'>+'"    { CfParser.OP_VEC_GT_S                  (CfParserUtil.next_lex lexbuf) }
  | "'<=+'"   { CfParser.OP_VEC_LE_S                  (CfParserUtil.next_lex lexbuf) }
  | "'>=+'"   { CfParser.OP_VEC_GE_S                  (CfParserUtil.next_lex lexbuf) }
  | "#"       { CfParser.OP_REPEAT                    (CfParserUtil.next_lex lexbuf) }
  | "'#'"     { CfParser.OP_VEC_REPEAT                (CfParserUtil.next_lex lexbuf) }
  | "::"      { CfParser.OP_CONS                      (CfParserUtil.next_lex lexbuf) }
  | "++"      { CfParser.OP_CONCAT                    (CfParserUtil.next_lex lexbuf) }
  | "'++'"    { CfParser.OP_VEC_CONCAT                (CfParserUtil.next_lex lexbuf) }
  | "\\"      { CfParser.OP_RETURN                    (CfParserUtil.next_lex lexbuf) }
  | "=="      { CfParser.OP_EQU                       (CfParserUtil.next_lex lexbuf) }
  | "!="      { CfParser.OP_NEQ                       (CfParserUtil.next_lex lexbuf) }
  | "'=='"    { CfParser.OP_VEC_EQU                   (CfParserUtil.next_lex lexbuf) }
  | "'!='"    { CfParser.OP_VEC_NEQ                   (CfParserUtil.next_lex lexbuf) }
  | "&"       { CfParser.OP_BW_AND                    (CfParserUtil.next_lex lexbuf) }
  | "'&'"     { CfParser.OP_VEC_AND                   (CfParserUtil.next_lex lexbuf) }
  | "^"       { CfParser.OP_BW_XOR                    (CfParserUtil.next_lex lexbuf) }
  | "'^'"     { CfParser.OP_VEC_XOR                   (CfParserUtil.next_lex lexbuf) }
  | "|"       { CfParser.OP_BW_OR                     (CfParserUtil.next_lex lexbuf) }
  | "'|'"     { CfParser.OP_VEC_OR                    (CfParserUtil.next_lex lexbuf) }
  | "&&"      { CfParser.OP_AND                       (CfParserUtil.next_lex lexbuf) }
  | "||"      { CfParser.OP_OR                        (CfParserUtil.next_lex lexbuf) }
  | "`&&`"    { CfParser.OP_PROP_AND                  (CfParserUtil.next_lex lexbuf) }
  | "`^^`"    { CfParser.OP_PROP_XOR                  (CfParserUtil.next_lex lexbuf) }
  | "`||`"    { CfParser.OP_PROP_OR                   (CfParserUtil.next_lex lexbuf) }
  | "`<->`"   { CfParser.OP_PROP_EQUIV                (CfParserUtil.next_lex lexbuf) }
  | "`->`"    { CfParser.OP_PROP_IMPLY                (CfParserUtil.next_lex lexbuf) }
  | "`U`"     { CfParser.OP_PROP_U                    (CfParserUtil.next_lex lexbuf) }
  | "then"    { CfParser.OP_THEN                      (CfParserUtil.next_lex lexbuf) }
  | "'then'"  { CfParser.OP_VEC_THEN                  (CfParserUtil.next_lex lexbuf) }
  | "'else'"  { CfParser.OP_VEC_ELSE                  (CfParserUtil.next_lex lexbuf) }
  | "<-"      { CfParser.OP_CONNECT_LEFT              (CfParserUtil.next_lex lexbuf) }
  | "->"      { CfParser.OP_CONNECT_RIGHT             (CfParserUtil.next_lex lexbuf) }
  | "="       { CfParser.OP_UNIFY                     (CfParserUtil.next_lex lexbuf) }

  | identifier    { CfParser.IDENTIFIER               (CfParserUtil.next_lex lexbuf) }

  | _         { CfParserUtil.error ("Unrecognized character: '" ^ (snd (CfParserUtil.next_lex lexbuf)) ^ "'");
                CfParser.LexerError (CfParserUtil.next_lex lexbuf) }

and comment = parse 
  | "(*"    { let _ = CfParserUtil.next_lex lexbuf in incr commentDepth; comment lexbuf }
  | "*)"    { let _ = CfParserUtil.next_lex lexbuf in decr commentDepth; if !commentDepth <> 0 then comment lexbuf else () }
  | string  { let _ = CfParserUtil.next_lex lexbuf in comment lexbuf }
  | eof     { CfParserUtil.error ("End of file without closing comment.") }
  | _       { let _ = CfParserUtil.next_lex lexbuf in comment lexbuf }

{
(* Post Code *)
}

