%{
open Expr_tseitin   
%}

/* description des lex�mes */

%token <int> VAR     
%token AND OR IMPLIES NOT
%token LPAREN RPAREN
%token EOF          

%left IMPLIES              /* associativit� gauche, precedence minimale */
%left AND                  /* associativit� gauche, precedence moyenne */
%left OR                   /* associativit� gauche, precedence forte */
%nonassoc NOT              /* pas d'associativit�, precedence maximale */


%start main          
%type <Expr_tseitin.formule> main   

%%

 /* Debut des regles de grammaire */

main:                       /* le point d'entr�e */
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

