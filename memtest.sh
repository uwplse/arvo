#!/bin/bash 

make

for f in examples/*.jpp ; do
    valgrind --leak-check=full --error-exitcode=1 ./arvo $f > $f.memlog 2>&1
    if [ $? -ne 0 ] ; then 
        echo memory error detected while executing file $f
        exit 1
    fi
    rm -f $f.memlog
done

echo no memory errors found!
