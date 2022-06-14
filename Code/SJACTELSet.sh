#!/bin/bash
#SBATCH --job-name=SJACTEL
#SBATCH --nodes=1 --ntasks=1
#SBATCH --time=2:00:00
#SBATCH --output=SJACTEL.out
#SBATCH --error=SJACTEL.err
#SBATCH --partition sixhour
#SBATCH --array=1
echo "$SLURM_ARRAY_TASK_ID"

Rscript
eche "Once more with quite a bit more feeling."
