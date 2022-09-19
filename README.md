## EuroHack'22 test cases
This repository contains small test cases (<5 min runtime) for the EuroHack'22 hackathon.

### Code
We will use the `develop` branch of Neko from the main repository:
https://github.com/ExtremeFLOW/neko
  
Developer documentation for Neko is available at: 
https://extremeflow.github.io/neko

### Instructions
Each case provides its instructions but relies on a CUDA installation of Neko. 
To build Neko, please follow the general build instructions given in 
the Neko README to generate the configure scripts (`regen.sh`). Then follow the 
general CUDA instructions (https://github.com/ExtremeFLOW/neko/discussions/540),
or the Piz Daint specific instructions (https://github.com/ExtremeFLOW/neko/discussions/601)
depending on the target system. 

**Note:** It's important to always configure Neko with a given installation prefix:
```console
> ./configure --prefix=/path/to/neko/installation ...additional flags...
```
otherwise, tools like `makeneko` or the `pkg-config` files will not work.

#### Known issues
* MPI I/O issues in MPI derived types if compiled with `gfortran < 12.x.y` and `mpich`
* Various issues with NVIDIA Fortran, due to its lack of certain Fortran 2008 features. 
