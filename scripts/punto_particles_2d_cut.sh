#! /bin/bash

if [ -z "$1" ]; then
    dirname=.
else
    dirname=$1
fi

rm ${dirname}/punto_particles_cut.dat 

for F in ${dirname}/mcf_*particles*.out; do
    
    printf "%s" $F
    perl  ~/project/trunk/mcf/scripts/particles_2d_cut.perl $F 'punto_particles_cut.dat'
    printf "\n" >> ${dirname}/punto_particles_cut.dat

done
