/* File parser.mly */

%token <int> INT
%token <float> FLOAT
%token PLUS MINUS TIMES DIV MODULO
%token PLUS_FLOAT MINUS_FLOAT TIMES_FLOAT
%token INT_FUN FLOAT_FUN
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS PLUS_FLOAT MINUS_FLOAT       /* lowest precedence */
%left TIMES DIV MODULO TIMES_FLOAT            /* medium precedence */
%nonassoc UPLUS UMINUS                        /* highest precedence */

%start main                                 /* the entry point */
%type <Asyntax.exp> main
%%
main:
  expr EOL                { $1 }
;

expr:
  INT                     { Int($1) }
  | FLOAT                    { Float($1) } 
  | INT_FUN LPAREN expr RPAREN {Appl("int", $3)}
  | FLOAT_FUN LPAREN expr RPAREN {Appl("float", $3)}
  | LPAREN expr RPAREN      { Appl("()", $2) }
  | expr PLUS expr          { Op("+" , $1 , $3) }
  | expr MINUS expr         { Op("-" , $1 , $3) }
  | expr TIMES expr         { Op("*" , $1 , $3) }
  | expr DIV expr           { Op("/" , $1 , $3) }
  | expr MODULO expr        { Op("%" , $1 , $3) }
  | expr PLUS_FLOAT expr    { Op("+." , $1 , $3)}
  | expr MINUS_FLOAT expr   { Op("-.", $1, $3)}
  | expr TIMES_FLOAT expr   { Op("*.", $1, $3)}
  | MINUS INT %prec UMINUS  {Int(- $2)}
  | MINUS FLOAT %prec UMINUS{Float(-. $2)}
  | PLUS INT %prec UPLUS    {Int($2)}
  | PLUS FLOAT %prec UPLUS  {Float($2)}
  | MINUS LPAREN expr RPAREN %prec UMINUS { Appl("-", $3) }
  | PLUS LPAREN expr RPAREN %prec UPLUS   { Appl("+", $3) }
;