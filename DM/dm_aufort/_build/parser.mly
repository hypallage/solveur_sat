%{

open Types 

(* rappel : dans types.ml, type entree = int list *)

%}

/* description des lexemes */

%token <int> INT       /* le lexeme INT a un attribut entier */
%token P C N F         /* lexeme de début de fichier et commentaires */
%token MOINS           /* gere les nombres négatifs */
%token EOF             /* gere la fin de fichier  */
%token ZERO            /* gere la fin de la clause */
%token EOL             /* gere les retours à la ligne notamment pour les commentaires */
%token TRUC            /* gere tout ce qu'il peut rester en commentaire */

%start main                /* "start" signale le point d'entree: c'est ici main */
%type <Types.entree> main  /* on doit donner le type du point d'entrée : ici une entree */

%%
/* Raisonnements bases sur les regles de grammaire */

main:                       /* le point d'entrée */
   commentaire_liste suite  {$2}
 | suite                    {$1} 
   
suite :
  P C N F INT INT EOL liste { $5::$6::$8 } 
 

liste:
   | EOF               { [] }
   | clause liste      { $1@$2 }
   | commentaire liste { $2 }

/* Un commentaire est composé de texte dont on ne s'occupe pas */
/*                et d'une fin de ligne                        */

commentaire_liste :
  | commentaire commentaire_liste { $2 } 
  | commentaire                   { $1 }

commentaire :
  | C texte  { $2 }

texte : 
   | C texte     { $2 }
   | P texte     { $2 }
   | N texte     { $2 }
   | F texte     { $2 }
   | MOINS texte { $2 }
   | ZERO texte  { $2 }
   | INT texte   { $2 }
   | TRUC texte  { $2 }
   | EOL         { [] }
    

clause:
  | ZERO EOL { [0] }		    
  | MOINS INT clause { (-$2)::$3 } 
  | INT clause       {  $1::$2 }
  | EOL { [] }       /* gestion des cas pathologiques */


