#!/bin/bash
#SBATCH --account=pawsey0001
#SBATCH --partition=debug
#SBATCH --nodes=1
#SBATCH --time=01:00:00
#SBATCH --exclusive

ml py-matplotlib/3.8.1 

date > time_python.log
echo "# X, Y, Z, Time(s)" >> time.log
# Testing for various lattice sizes 
for n in {10,20,30,40,50,60,70,80,90,100,150,200}; do 
    srun -N 1 -n 1 -c 1 python create_node_dict.py  $n $n $n >> time.log 
done
