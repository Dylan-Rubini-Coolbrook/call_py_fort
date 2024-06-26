#!/bin/bash

module purge
# set-up/source spack environment variables
unset LD_LIBRARY_PATH
# set-up/source spack environment variables
source /datapart1/spack/share/spack/setup-env.sh
spack clean
spack unload --all

# load modules from spack
module load gcc-11.2.0-gcc-7.3.0-67ocnsa

micromamba deactivate
micromamba activate tf-cpu-source2

export PYTHONPATH=$(pwd):$PYTHONPATH

# NOTE: installation location of callpy_mod.mod and callpy_mod.so is in
# the directories /usr/local/include and /user/local/lib
gfortran -O3 -march=native -mtune=native -I/usr/local/include  -Wl,-rpath=/home/orie3565/micromamba/envs/tf-gpu/lib/ -Wl,-rpath=/usr/local/lib -L/usr/local/lib test_performance_fort.f90 -lcallpy

time ./a.out

time python ./main_python.py
