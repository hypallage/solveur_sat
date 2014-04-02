#!/bin/bash

# preparation du fichier comparaison.dat: on l'enleve s'il existe, 
# on ecrit l'en-tete des colonnes
rm -f nb_satisf.dat

min=6
max=6
nb_formules=3

# une boucle for:
# la variable entree parcourt toutes les valeurs que renvoie la commande
# seq 25 40
# on pourrait aussi faire  "for entree in `ls exemples/`" pour parcourir
# tous les noms de fichier contenus dans le sous-repertoire exemples/.
# au passage, la syntaxe `bla` est utilisee pour designer "la valeur
# renvoyee par l'execution de la commande bla"

for entree in `seq $min $max`; do

nb_clauses=$((entree*2))
echo $nb_clauses

for i in `seq 1 $nb_formules`; do

echo $nb_clauses
./calc $entree $nb_clauses 
minisat ex.cnf solution.txt

if grep SAT solution.txt 
    then
	 if grep UNSAT solution.txt 
	 then (echo "unsat")
	 else (echo "sat"
               echo "1" >> nb_satisf.dat)
	 fi
    else echo "invalide" 
fi

done

deux_fois_nb_satisf=$(wc -m nb_satisf.dat) 

echo $deux_fois_nb_satisf

done

# Le nombre de clauses satisfiables est donc le nombre de 1 dans le fichier, 
# soit la moitie du nombre de caracteres ( il faut compter les retours a la
# ligne)	

# a noter que vous pouvez passer un ou plusieurs arguments a ce
# script, auxquels vous ferez reference par $1, $2, etc. Par exemple,
# vous pourrez taper "bash run-tests.sh toto.dat (pour indiquer le nom
# du fichier rassemblant les resultats des tests), et faire dans ce
# fichier 'echo "argument Fibonacci Fibonacci-memo" >> $1



