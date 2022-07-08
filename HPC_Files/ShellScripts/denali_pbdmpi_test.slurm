#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=workq
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=0-01:00:00   
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40      
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout
#SBATCH --mail-type=ALL
#SBATCH --mail-user=candrews@usgs.gov

# Add module
#module list
module switch PrgEnv-cray/6.0.5 PrgEnv-gnu/6.0.5
module load cray-R/3.6.1 gcc/8.3.0 cray-mpich/7.7.11
module load geos/3.8.1 gdal/3.0.4 proj/6.2.1 udunits2/2.2.26 cray-netcdf-hdf5parallel

srun date
mpiexec Rscript Simple_weather_pbdmpitest.R
srun date