#!/bin/bash -l
#SBATCH --job-name modelruns_arima_MAR_A
#SBATCH --partition=cpu
#SBATCH --time=010:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=astears@usgs.gov
#SBATCH --account=swbsc
#SBATCH --output=output/modelruns_arima_MAR_A_ex01_%A_%a.out
#SBATCH --array=0-1

echo "SLURM_JOB_ID:" $SLURM_JOB_ID
echo "SLURM_JOB_NAME:" $SLURM_JOB_NAME
echo "SLURM_JOB_NODELIST:" $SLURM_JOB_NODELIST
echo "SLURM_ARRAY_TASK_ID:" $SLURM_ARRAY_TASK_ID

module load cray-R  

cd /caldera/hovenweep/projects/usgs/ecosystems/sbsc/DrylandEcologyTeam/gridSTDF/temp


Rscript --vanilla modelruns_arima_MAR_A.R $SLURM_ARRAY_TASK_ID

