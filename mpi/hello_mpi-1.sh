#!/bin/bash

module purge
module load rhel7/default-peta4
module load gcc

PROGRAM="hello_mpi-1.f90"

mpifc "$PROGRAM" -o binary

NAME=$(echo "$1" | sed 's/\..*//')

srun -A training-cpu -p skylake --time=1:00 --reservation=hpc_academy_day3 --ntasks=2 --nodes=2 binary > $NAME.out
