#!/bin/bash

#SBATCH -A swbsc
#SBATCH -p cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=12:00:00
#SBATCH --mem=4GB
#SBATCH -N 1
#SBATCH -n 10
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcologyTeam/gridSTDF/HPC_Files/Outs/gridSTDF_%j.out
#SBATCH --error=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcologyTeam/gridSTDF/HPC_Files/Outs/gridSTDF_%j.err
#SBATCH --mail-use=astears@usgs.gov
#SBATCH --mail-type=ALL

cd /caldera/hovenweep/projects/usgs/ecosystems/sbsc/DrylandEcologyTeam/gridSTDF

module load cray-R-netcdf proj/9.3.0 geos/3.12.1

#srun Rscript main/implementation/01_main-simulation-runner.R

