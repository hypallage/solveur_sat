#!/bin/bash
 
nbbooks=0
fin=3
while [ "$nbbooks" != "$fin" ] 
    do
	nbbooks=$((nbbooks + 1))
	echo "$nbbooks dans la boucle"
    done
echo "$nbbooks dehors a boucle"
