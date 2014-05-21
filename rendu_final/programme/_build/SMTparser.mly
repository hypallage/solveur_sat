%{

%}

/* description des lexèmes */

%token <int> INT       /* le lexeme INT a un attribut entier */
%token EOF             /* gere la fin de fichier             */
%token EOL             /* gere les retours à la ligne notamment pour les commentaires */
%token TRUC            /* gere tout ce qu'il peut rester en commentaire */
%token OU	       /* les lexemes suivant parle d'eux même */
%token EGAL
%token DISEGAL
%token NON

%start main                
%type <int list> main  /* on renvoie une liste d'entier */

%%
/* Raisonnements bases sur les regles de grammaire */
/*               Regles de grammaire               */

main:                       /* le point d'entrée */
  | NON INT EGAL INT main       	{ (-$2::(-$4::$5)) }	/*Un non donc on change */
  | NON INT DISEGAL EGAL INT main     	{ ($2::($5::$6))   }	/*Deux non qui s'annule */
  | INT EGAL INT main            	{ ($1::($3::$4))   }	/*Aucun non */
  | INT DISEGAL EGAL INT main        	{ (-$1::(-$4::$5)) }	/*Un non donc on change */
  | OU main 				{  $2              }
  | EOL main				{  (0 :: $2)       }
  | EOF                          	{[]                }



