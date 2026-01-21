%{
  (* SMV Parser for propositional formulae *)
  open Ltl

  (* Helper to apply unary operations *)
  let unary_op op arg =
    match op with
    | "!" -> PNot arg
    | "G" ->
        (* For temporal operators, just return the argument for now *)
        arg
    | "F" -> arg
    | "X" -> arg
    | _ -> arg

  (* Helper to apply binary operators *)
  let binary_op op left right =
    match op with
    | "&" -> PAnd [left; right]
    | "|" -> POr [left; right]
    | "->" -> PImply (left, right)
    | "<->" -> PIff (left, right)
    | _ -> left
%}

/* Terminal symbols */
%token TRUE FALSE
%token AND OR NOT
%token IMPLIES IFF
%token G F X U R W
%token EQ NEQ LT LE GT GE
%token PLUS MINUS MULT DIV MOD
%token LPAREN RPAREN
%token <string> IDENT
%token EOF

/* Start symbol */
%start formula
%type <Ltl.t> formula

/* Operator precedence and associativity (low to high) */
%left IFF
%left IMPLIES
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MULT DIV MOD
%right NOT
%right G F X
%left U R W

%%

formula:
  | expr EOF { $1 }

expr:
  | expr AND expr       { binary_op "&" $1 $3 }
  | expr OR expr        { binary_op "|" $1 $3 }
  | expr IMPLIES expr   { binary_op "->" $1 $3 }
  | expr IFF expr       { binary_op "<->" $1 $3 }
  | expr U expr         { binary_op "U" $1 $3 }
  | expr R expr         { binary_op "R" $1 $3 }
  | expr W expr         { binary_op "W" $1 $3 }
  | NOT expr            { unary_op "!" $2 }
  | G expr              { unary_op "G" $2 }
  | F expr              { unary_op "F" $2 }
  | X expr              { unary_op "X" $2 }
  | expr EQ expr        { binary_op "=" $1 $3 }
  | expr NEQ expr       { binary_op "!=" $1 $3 }
  | expr LT expr        { binary_op "<" $1 $3 }
  | expr LE expr        { binary_op "<=" $1 $3 }
  | expr GT expr        { binary_op ">" $1 $3 }
  | expr GE expr        { binary_op ">=" $1 $3 }
  | expr PLUS expr      { binary_op "+" $1 $3 }
  | expr MINUS expr     { binary_op "-" $1 $3 }
  | expr MULT expr      { binary_op "*" $1 $3 }
  | expr DIV expr       { binary_op "/" $1 $3 }
  | expr MOD expr       { binary_op "mod" $1 $3 }
  | primary             { $1 }

primary:
  | TRUE                { PTrue }
  | FALSE               { PFalse }
  | IDENT               { PAtom $1 }
  | LPAREN expr RPAREN  { $2 }
