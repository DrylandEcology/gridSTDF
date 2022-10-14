#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=0-01:00:00   
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --chdir=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/main/implementation/pbdncdf4/
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout

# Add module
module load netcdf-c/4.9.0
module load R/4.2.0

srun date
echo "**** Writing in Serial ..."
srun Rscript ncwrite_ser.r
echo "**** Reading in Serial ..."
srun Rscript ncread_ser.r
echo "**** Writing in Parallel ..."
srun Rscript ncwrite_par.r
echo "**** Reading in Parallel ..."
srun Rscript ncread_par.r
srun date
