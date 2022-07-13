#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=0-01:00:00   
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=40      
#SBATCH --chdir=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout
#SBATCH --mail-type=ALL
#SBATCH --mail-user=candrews@usgs.gov

# Add module
#module list
module load R/3.6.3 gcc/8.2.0 openmpi/4.0.3
module load geos/3.8.1 gdal/3.0.4 proj/6.2.1 netcdf parallel-netcdf/1.12.1  

srun date
mpirun Rscript main/implementation/Simple_weather_pbdmpi_test.R
srun date