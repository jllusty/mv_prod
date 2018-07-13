#!/bin/bash -x

#SBATCH --job-name="mv_prod"
#SBATCH --ntasks-per-node="1"
#SBATCH --nodes="1"
#SBATCH --ntasks="1"
#SBATCH --exclusive
#SBATCH --export=ALL
#SBATCH --account=math540s18
#SBATCH --time=00:01:00
#SBATCH -o mv_prod.out
#SBATCH -e mv_prod.err

module purge
module load StdEnv
module load impi/intel/latest
srun ./mv_prod_exe
