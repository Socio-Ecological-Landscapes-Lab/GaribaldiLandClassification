#!/bin/bash
#SBATCH --job-name=train_job
#SBATCH --account=def-nbl
#SBATCH --output=log_train.txt
#SBATCH --error=log_error.txt
#SBATCH --time=01:00:00
#SBATCH --mem=64G

module load StdEnv/2020 gcc/9.3.0 gdal/3.5.1 udunits/2.2.28 r/4.2.2

Rscript /project/def-nbl/come/codes/TrainRangerWorldCover.r
