# pour visualiser le dessin trace par ce script gnuplot, taper
# gnuplot -persist script-plot.p
#  (en s`assurant que le fichier comparaison.dat est present dans le repertoire)

reset

### decommenter les 2 lignes ci-dessous pour engendrer un fichier pdf
### plutot qu`un dessin a l`ecran
set term pdfcairo
set output "normal_rand_unsat.pdf" # le nom du fichier qui est engendre

set title "Comparaison des temps d'execution moyens de tous les algorithmes"
set xlabel "nombre de variables"
set ylabel "Temps d'execution"


# Dessin en joignant des points
set style data points

set pointsize 0.3   # la taille des points
set logscale y

# on trace deux courbes: avec les colonnes 1 et 2, avec les colonnes 1 et 3
# a chaque fois, le nom de la courbe est lu en tete de colonne
plot "comparaison.dat" using 1:($1<150 ? $2 : 1/0) title columnheader(2), \
     "comparaison.dat" using 1:3 title columnheader(3), \
     "comparaison.dat" using 1:4 title columnheader(4), \
     "comparaison.dat" using 1:5 title columnheader(5), \
     "comparaison.dat" using 1:6 title columnheader(6)

