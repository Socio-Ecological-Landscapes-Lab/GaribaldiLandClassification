#!/bin/bash
#SBATCH --job-name=classification_job
#SBATCH --account=def-nbl
#SBATCH --output=log_classification.txt
#SBATCH --error=error_classification.txt
#SBATCH --time=05:00:00
#SBATCH --cpus-per-task=14
#SBATCH --mem-per-cpu=10G
#SBATCH --tmp=20G
#SBATCH --mail-user=cbeauquier@uvic.ca
#SBATCH --mail-type=FAIL

module load StdEnv/2020 gcc/9.3.0 gdal/3.5.1 udunits/2.2.28 r/4.2.2

Rscript /project/def-nbl/come/codes/Compute_SupervisedClassification_Planet8B.r
