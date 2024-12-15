#!/bin/sh


TF_VERSION="2"
TF_ENV_NAME="tf-cpu-source${TF_VERSION}"

micromamba deactivate
micromamba activate ${TF_ENV_NAME}

export FC=gfortran
export F90=gfortan
export F77=gfortran
export CC=gcc
export CXX=g++
export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Determine the OS type
os_type=$(uname -s)
cpu_arch=$(uname -m)

pre-commit install

if [ "$os_type" = "Darwin" ] && [ "$cpu_arch" = "arm64" ]; then
    echo "This is macOS."
    FLAGS="armv9.2-a"

elif [ "$os_type" = "Linux" ]; then
	echo "This is Linux"
    FLAGS="-march=native -mtune=native"
else
	exit 1
fi

sudo rm -rf build
mkdir build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_FLAGS="-O3 ${FLAGS} -Wl,-rpath ${CONDA_PREFIX}/lib/"
make
make test

sudo make install

cd ..
