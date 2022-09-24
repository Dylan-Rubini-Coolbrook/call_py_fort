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

conda activate ct-build

export FC=gfortran
export F90=gfortan
export F77=gfortran

sudo rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_Fortran_FLAGS="-O3"
make
make test

sudo make install

cd ..