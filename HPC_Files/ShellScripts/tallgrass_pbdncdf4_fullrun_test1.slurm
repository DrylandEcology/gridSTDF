#!/bin/bash
#SBATCH --account=swbsc
#SBATCH --partition=cpu
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=6-0:00:00   
#SBATCH --nodes=10
#SBATCH --ntasks=360
#SBATCH --chdir=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF_Projects/gridSTDF/HPC_Files/Outs/gridSTDF_%j.stdout

# Add module
module load netcdf-c/4.9.0
module load R/4.2.0
module load geos/3.8.1 gdal/3.0.4 proj/6.2.1 

srun date

#srun Rscript projects/05-Setup-futureMonthly-netCDFs/Create-template-netCDFs.R
srun Rscript main/implementation/Parallel_Test_Oct2022.R

srun date
