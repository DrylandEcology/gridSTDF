#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=0-01:00:00   
#SBATCH --nodes=1
#SBATCH --ntasks=10
##SBATCH --cpus-per-task=40      
#SBATCH --chdir=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout
##SBATCH --mail-type=ALL
##SBATCH --mail-user=candrews@usgs.gov

# Add module
##module purge
##module load slurm gcc openmpi/4.0.3 R/4.1.3 hdf5/1.10.6 netcdf-c/4.8.1

## Lopaka's test modules/env
module load R
module load hdf5-1.12.2-gcc-8.2.0-6buq556 netcdf-c-4.8.1-gcc-8.2.0-zwlaygw

srun date
#mpirun Rscript main/implementation/Simple_weather_pbdmpi_test.R
#mpirun Rscript main/implementation/pbdMPI-hello.R
#mpirun Rscript main/implementation/writeN-read1-test.R
srun Rscript main/implementation/writeN-read1-test.R
srun date
