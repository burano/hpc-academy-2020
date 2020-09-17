#!/bin/bash

module purge
module load rhel7/default-peta4
module load gcc

mpifc hello_mpi.f90 -o hello_mpi

srun -A training-cpu -p skylake --time=1:00 --reservation=hpc_academy_day3 --ntasks=64 --nodes=2 hello_mpi > hello_mpi.out
