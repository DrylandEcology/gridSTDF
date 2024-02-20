#!/bin/sh
#SBATCH --time=02:00:00
##SBATCH --job-name=pbdNCDF4_test
#SBATCH --output=pbdNCDF4_test-%j.out
#SBATCH --error=pbdNCDF4_test-%j.err
#SBATCH --account=swbsc
#SBATCH -p cpu
#SBATCH -N 1
#SBATCH -n 10
#SBATCH --mail-type=FAIL,TIME_LIMIT
##SBATCH --mail-astears@agency.gov

module load R/3.6.3 gcc/8.2.0 openmpi/4.0.3 netcdf-c/4.8.1 parallel-netcdf/1.12.1 

mpirun Rscript parallelTest_pbdNetCDF.R