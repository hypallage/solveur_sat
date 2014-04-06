#!/bin/bash

# preparation du fichier comparaison.dat: on l'enleve s'il existe, 
# on ecrit l'en-tete des colonnes
rm -f comparaison.dat
echo "nb_var satisf?" >> comparaison.dat

SATISF=0
INSATISF=0

min=6
max=10

# une boucle for:
# la variable entree parcourt toutes les valeurs que renvoie la commande
# seq 25 40
# on pourrait aussi faire  "for entree in `ls exemples/`" pour parcourir
# tous les noms de fichier contenus dans le sous-repertoire exemples/.
# au passage, la syntaxe `bla` est utilisee pour designer "la valeur
# renvoyee par l'execution de la commande bla"

for entree in `seq $min $max`; do

echo $entree | ./calc $entree  20 
echo $entree | minisat ex.cnf solution.txt

if grep SAT solution.txt 
    then
	 if grep UNSAT solution.txt 
	 then (echo $entree "non" >> ./comparaison.dat 
                  let "INSATISF= $INSATISF+1"
                  echo $INSATISF )
	 else (echo $entree "oui" >> ./comparaison.dat 
                  let "SATISF = $SATISF + 1" 
                  echo $SATISF)
	 fi
    else echo $entree "invalide" >> ./comparaison.dat 
fi

if [ $entree -eq $max ] ;
 then echo $SATISF $INSATISF >> ./comparaison.dat
fi

done




# a noter que vous pouvez passer un ou plusieurs arguments a ce
# script, auxquels vous ferez reference par $1, $2, etc. Par exemple,
# vous pourrez taper "bash run-tests.sh toto.dat (pour indiquer le nom
# du fichier rassemblant les resultats des tests), et faire dans ce
# fichier 'echo "argument Fibonacci Fibonacci-memo" >> $1