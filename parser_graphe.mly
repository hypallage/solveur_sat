%{

%}

/* description des lexèmes */

%token <int> INT       /* le lexeme INT a un attribut entier */
%token P C N F         /* lexeme de début de fichier         */
%token E               /* lexeme de lecture d'une arrete     */
%token EDGE            /* gere les nombres négatifs          */
%token EOF             /* gere la fin de fichier             */
%token ZERO            /* gere la fin de la clause           */
%token EOL             /* gere les retours à la ligne notamment pour les commentaires */
%token TRUC            /* gere tout ce qu'il peut rester en commentairee */

%start main                /* "start" signale le point d'entree: c'est ici main */
%type <int list> main  /* on _doit_ donner le type du point d'entrée */

%%
/* Raisonnements bases sur les regles de grammaire */
/*               Regles de grammaire               */

main:                       /* le point d'entrée */
   commentaire_liste suite  {$2}
 | suite                    {$1} 
   
suite :
  P E F INT INT EOL liste_arrete { $4::$5::$7 } 
 

liste_arrete:
   | EOF			{ [] }
   | clause liste_arrete	{ $1@$2 }
   | commentaire liste_arrete	{ $2 }

/* Un commentaire est composé de texte dont on ne s'occupe pas */
/*                et d'une fin de ligne                        */

commentaire_liste :
  | commentaire commentaire_liste { $2 } 
  | commentaire                   { $1 }

commentaire :
  | C texte  { $2 }

texte :
   | P texte     { $2 } 
   | C texte     { $2 }
   | F texte     { $2 }
   | E texte     { $2 }
   | EDGE texte  { $2 }
   | ZERO texte  { $2 }
   | INT texte   { $2 }
   | TRUC texte  { $2 }
   | EOL         { [] }
    

clause:			    
  | E INT INT clause { ($2::($3::$4)) } 
  | EOL                 { [] }       /* gestion des cas pathologiques */


