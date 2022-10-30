(* File lexer.mll *)
{
open Parser       (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
    [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { EOL }
    | ['0'-'9']* '.' ['0' - '9']+ | ['0'-'9']+ '.' ['0' - '9']* as lxm { FLOAT(float_of_string lxm) }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "+."           {PLUS_FLOAT}
    | "-."           {MINUS_FLOAT}
    | "*."           {TIMES_FLOAT}
    | '+'            { PLUS }
    | '-'            { MINUS }
    | '*'            { TIMES }
    | '/'            { DIV }
    |"int"           { INT_FUN }
    |"float"         { FLOAT_FUN }
    | '('            { LPAREN }
    | ')'            { RPAREN }
    | '%'            { MODULO }
    | eof            { raise Eof }