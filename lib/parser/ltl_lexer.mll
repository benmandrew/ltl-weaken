{
  (* Lexer for parsing LTL formulae *)
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter (letter | digit)*

rule token = parse
  | whitespace     { token lexbuf }
  | "(*"           { comment lexbuf }
  | "TRUE"         { Ltl_parser.TRUE }
  | "FALSE"        { Ltl_parser.FALSE }
  | "true"         { Ltl_parser.TRUE }
  | "false"        { Ltl_parser.FALSE }
  (* Logical operators *)
  | "&&"           { Ltl_parser.AND }
  | '&'            { Ltl_parser.AND }
  | "||"           { Ltl_parser.OR }
  | '|'            { Ltl_parser.OR }
  | '!'            { Ltl_parser.NOT }
  | "->"           { Ltl_parser.IMPLIES }
  | "<->"          { Ltl_parser.IFF }
  (* Temporal operators *)
  | "next"         { Ltl_parser.X }
  | 'G'            { Ltl_parser.G }
  | 'F'            { Ltl_parser.F }
  | 'X'            { Ltl_parser.X }
  | 'W'            { Ltl_parser.W }
  | "[]"           { Ltl_parser.G }  (* Alias for G *)
  | "<>"           { Ltl_parser.F }  (* Alias for F *)
  | 'U'            { Ltl_parser.U }
  | 'R'            { Ltl_parser.R }
  (* Comparisons *)
  | "=="           { Ltl_parser.EQ }
  | "!="           { Ltl_parser.NEQ }
  | '='            { Ltl_parser.EQ }
  | "<="           { Ltl_parser.LE }
  | ">="           { Ltl_parser.GE }
  | '<'            { Ltl_parser.LT }
  | '>'            { Ltl_parser.GT }
  (* Arithmetic *)
  | '+'            { Ltl_parser.PLUS }
  | '-'            { Ltl_parser.MINUS }
  | '*'            { Ltl_parser.MULT }
  | '/'            { Ltl_parser.DIV }
  | "mod"          { Ltl_parser.MOD }
  (* Delimiters *)
  | '('            { Ltl_parser.LPAREN }
  | ')'            { Ltl_parser.RPAREN }
  | ':'            { token lexbuf }  (* Skip colon *)
  | '.'            { token lexbuf }  (* Skip dot *)
  | ','            { token lexbuf }  (* Skip comma *)
  | ';'            { token lexbuf }  (* Skip semicolon *)
  (* Identifiers and numbers *)
  | ident as id    { Ltl_parser.IDENT id }
  | eof            { Ltl_parser.EOF }
  | _              { token lexbuf }  (* Skip unexpected characters *)

and comment = parse
  | "*)"           { token lexbuf }
  | _              { comment lexbuf }
