#!/bin/bash
#SBATCH --account=def-nbl         # Remplace par ton allocation de projet
#SBATCH --job-name=install_r_packages
#SBATCH --output=install_r_packages_%j.log  # Le %j sera remplacé par l'ID du job
#SBATCH --time=01:00:00                 # Durée maximale du job
#SBATCH --cpus-per-task=4              # Nombre de cœurs CPU
#SBATCH --mem=16G                      # Mémoire RAM allouée

# Charger les modules requis
module load  StdEnv/2020  gcc/9.3.0  udunits/2.2.28  gdal/3.5.1  r/4.2.2

# Créer un dossier de packages R perso
mkdir -p $HOME/R/x86_64-pc-linux-gnu-library/4.2
export R_LIBS="$HOME/R/x86_64-pc-linux-gnu-library/4.2:$R_LIBS"

# Installer les packages
R -e "install.packages('caret', repos='https://mirror.csclub.uwaterloo.ca/CRAN/', dependencies=TRUE)"
