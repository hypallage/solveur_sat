#!/bin/bash

rm -f comparaison.dat
echo "nb_var normal rand dlis" >> comparaison.dat
echo "toto"
for i in `seq 1 30`; do

nb_sommet=$(echo "2*$i" | bc) # On compare les algos pour ces valeurs de nb_var
echo "$nb_sommet"
a=0.00
d=0.00
e=0.00
for nb_fois in `seq 1 50`; do # Moyenne sur un certain nombre de formules 

cd "generateur_graphe" 
 ./gen $nb_sommet 50 # On cree la formule
cp ex.col ../version_op # On la copie dans le repertoire version_op

  # On se place dans le répertoire du programme
  cd ../version_op 
  # On calcule les differents temps et on les ajoute 
  echo $entree | /usr/bin/time -f'%U' -o /tmp/toto.txt ./resol -graphe $i ex.col
  TEMPS1=`cat /tmp/toto.txt`
  a=$(echo "scale=3; $TEMPS1 + $a" | bc)
  echo "normal"

  echo $entree | /usr/bin/time -f'%U' -o /tmp/toto.txt ./resol -rand -graphe $i ex.col 
  TEMPS2=`cat /tmp/toto.txt`
  d=$(echo "scale=3; $TEMPS2 + $d" | bc)
  echo "rand"
  echo $entree | /usr/bin/time -f'%U' -o /tmp/toto.txt ./resol -dlis -graphe $i ex.col
  TEMPS5=`cat /tmp/toto.txt`
  e=$(echo "scale=3; $TEMPS5 + $e" | bc)
  echo "dlis"
  cd ..

# pour finir, on ajoute une ligne (6 colonnes) au fichier ./comparaison.dat
done

# On fait la moyenne

a=$(echo "scale=2; $a/50" | bc) 
d=$(echo "scale=2; $d/50" | bc)
e=$(echo "scale=2; $e/50" | bc)

echo $nb_sommet $a $d $e >> ./comparaison.dat 

done

