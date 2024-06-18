#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=6-0:00:00   
#SBATCH --nodes=10
#SBATCH --ntasks=360
#SBATCH --chdir=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcologyTeam/gridSTDF/
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcologyTeam/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout

# Add module
module module load cray-R-netcdf

#srun date

#srun Rscript projects/05-Setup-futureMonthly-netCDFs/Create-template-netCDFs.R
srun Rscript main/implementation/01_main-simulation-runner.R

#srun date
