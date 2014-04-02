%{
open Expr   
%}

/* description des lexèmes */

%token <int> VAR     
%token AND OR IMPLIES NOT
%token LPAREN RPAREN
%token EOF          

%left IMPLIES              /* associativité gauche, precedence minimale */
%left AND                  /* associativité gauche, precedence moyenne */
%left OR                   /* associativité gauche, precedence forte */
%nonassoc NOT              /* pas d'associativité, precedence maximale */


%start main          
%type <Expr.formule> main   

%%

 /* Debut des regles de grammaire */

main:                       /* le point d'entrée */
    form EOF                { $1 }  
;
form:			  
  | VAR                  { Var $1 }
  | LPAREN form RPAREN   { $2 } 
  | form AND form        { And($1,$3) }
  | form OR form         { Or($1,$3) }
  | form IMPLIES form    { Implies($1,$3) }
  | NOT form             { Not($2)}
;

