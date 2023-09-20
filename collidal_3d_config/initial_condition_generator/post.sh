#!/bin/bash

set -e
set -u

root=$(pwd)
for d in $(ls -d NUM_PART_*); do
    cd ${d}
    printf "processing: %s\n" ${d} > "/dev/stderr"
    for c in $(ls particles/mcf*); do
	awk '{print $1, $2, $3, 1.0}' ${c}
	printf "\n"
    done > punto.dat
    for c in $(ls mcf_init_part*); do
	awk '{print $1, $2, $3, 1.0}' ${c}
	printf "\n"
    done > pre.dat
    cd "${root}"
done

# punto -bg white -r -z 1:3:2:7 -B 0:0:0:8:8:8 COLR9.510e-01COLZ6.046e+00/punto.dat 
