#!/bin/bash

srcfile0='timing_basic_cmp_cruz.f90'
srcfile1='scaling_operations_cmp_cruz.f90'
srcfile2='timing_sort_cmp_cruz.f90'
script='scaling_operations_cmp_cruz.gplot'
output1='assign.dat'
output2='readout.dat'
output3='addition.dat'
output4='multiplication.dat'
output5='sorting.dat'

if [[ -f $srcfile0 ]]
then
    echo '------------ASSIGNMENT #1------------'
    gfortran -o out0.exe $srcfile0
    ./out0.exe
else ${srcfile0} 'file not in working directory'
fi
    

if [[ -f $srcfile1 ]]
then
    echo '------------ASSIGNMENT #2------------'
    gfortran -o out1.exe $srcfile1
    ./out1.exe
else
    echo ${srcfile1} 'file not in working directory'
fi

if [[ -f $srcfile2 ]]
then
    gfortran -o out2.exe $srcfile2
    ./out2.exe
else 
    echo ${srcfile2} 'file not in working directory'
fi

if [[ -f $script && -f $output1 && -f $output2 && -f $output3 && -f $output4 && -f $output5 ]]
then
    gnuplot $script
else
    echo "Some files are missing in the working directory"
fi
