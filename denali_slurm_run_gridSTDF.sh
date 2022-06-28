#!/bin/bash
#SBATCH --account=swbsc
##SBATCH --partition=short
#SBATCH --job-name=gridSTDF_Test
#SBATCH --time=1-00:00:00   
##SBATCH --dependency=singleton
#SBATCH --nodes=1
##SBATCH --ntasks=5
#SBATCH --cpus-per-task=40      
##SBATCH -D=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/                  
#SBATCH --output=/caldera/projects/usgs/ecosystems/swbsc/DrylandEcohydrologyLab/gridSTDF/Outputs/gridSTDF%J.stdout
#SBATCH --mail-type=ALL
#SBATCH --mail-user=candrews@usgs.gov   #SBATCH --mem-per-cpu=1200

# start postgresql
pg_ctl start -D mylocal_db2

# Add module
module load gcc/8.3.0 cray-R/3.6.1
module load geos/3.8.1 gdal/3.0.4 proj/6.2.1
module load udunits2/2.2.26 cray-netcdf-hdf5parallel

# Run Scripts 
#export R_LIBS=/home/cma393/R/x86_64-redhat-linux-gnu-library/3.5/:$R_LIBS

srun date
srun Rscript --vanilla gridSTDF/implementation/Parallel_Test.R
srun date