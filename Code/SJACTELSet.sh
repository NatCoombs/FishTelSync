#!/bin/bash
#SBATCH --job-name=SJACTEL
#SBATCH --nodes=1 --ntasks=1
#SBATCH --time=2:00:00
#SBATCH --output=SJACTEL.%A_%a.out
#SBATCH --error=SJACTEL.%A_%a.err
#SBATCH --partition sixhour
#SBATCH --array=1-2
#SBATCH --mail-type=BEGIN,END,FAIL

echo "$SLURM_ARRAY_TASK_ID"

Rscript SJClusterArray.R
eche "Once more with quite a bit more feeling."
