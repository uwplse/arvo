#!/bin/bash

make

GOOD=0

for f in examples/good/*.arvo ; do
    ./arvo $f > $f.log 2>&1
    if grep ERROR $f.log ; then
        echo test failed: $f
        GOOD=1
    else
        rm -f $f.log
    fi
done

if [ $GOOD -eq 0 ] ; then echo all tests passed!; fi

exit $GOOD
