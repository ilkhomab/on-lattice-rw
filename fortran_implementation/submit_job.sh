#!/bin/bash
#SBATCH --account=pawsey0001
#SBATCH --partition=debug
#SBATCH --nodes=1
#SBATCH --time=01:00:00

date > time.log
echo "# X, Y, Z, num_points,  Time(s)" >> time.log
# Testing for various lattice sizes 
for n in {10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200}; do 
    srun -N 1 -n 1 -c 1 ./opt3 $n $n $n >> time.log 
done
