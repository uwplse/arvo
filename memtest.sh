#!/bin/bash 

make

if [ "$(uname)" = "Darwin" ] ; then
    SUPPRESSIONS="--suppressions=./osx.supp"
else
    SUPPRESSIONS=""
fi

for f in examples/good/*.arvo examples/bad/*.arvo ; do
    valgrind --leak-check=full --error-exitcode=1 $SUPPRESSIONS ./arvo $f > $f.memlog 2>&1
    if [ $? -ne 0 ] ; then 
        echo memory error detected while executing file $f
        cat $f.memlog
        exit 1
    fi
    rm -f $f.memlog
done

echo no memory errors found!
