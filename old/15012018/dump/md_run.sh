#!/bin/bash

gfortran -o rahman_initial.exe rahman_initial.f90
./rahman_initial.exe

gfortran -c rahman_aux.f90 rahman_param.f90
gfortran rahman_aux.o rahman_param.o rahman_argon.f90
./a.out
