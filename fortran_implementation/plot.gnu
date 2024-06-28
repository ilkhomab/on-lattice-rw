#!/usr/bin/gnuplot --persist
##################################################
#Settings and color definitions
#################################################
if (!exists('COL0'))        COL0='#000000' #black
if (!exists('COL1'))        COL1='#ff0000' #red
if (!exists('COL2'))        COL2='#008800' #green
if (!exists('COL3'))        COL3='#0000aa' #blue
if (!exists('COL4'))        COL4='#ff00ff' #pink
if (!exists('COL5'))        COL5='#7e2f8e' #purple
if (!exists('COL6'))        COL6='#edb120' #yellow
if (!exists('COL7'))        COL7='#d95319' #orange
if (!exists('COL8'))        COL8='#884400' #brown
if (!exists('COL9'))        COL9='#4dbeee' #light-blue
if (!exists('COL10'))       COL10='#a2142f' #dark-red
if (!exists('COL11'))       COL11='#21908d' #blue-green
if (!exists('COL12'))       COL12='#6495ED' #aqua

#set term postscript enhanced eps color solid "Times-Roman,16" #size 8.5cm,12cm
set terminal png 
set output "box.png"
#set point sizes
my_ps=0.60

set ylabel "compute time (secs)" offset 1.5,0
set log y
set xtics 1
set yr [*:*]
set xr [*:*]
set xtics 20
set mxtics 4
set key samplen 1.5
set key Left reverse
set key bottom right
set format x
set xlabel "X=Y=Z"
#set mytics
#set label "WP-CCC: 2C" at 0.5, 40000


p 'time.log' u 1:5 title "Fortran: 1-core" lw 2 pt 10 lc rgb COL4 ps my_ps w lp ,\
  '../time_python.log'  u 1:4 title "Python: 1-core" lw 2 pt 6 lc rgb COL2 ps my_ps w lp 


#!epstopdf performance.eps
#!open -a TeXShop performance.pdf
  


