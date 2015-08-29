#!/bin/bash

cargo build

GOOD=0

for f in examples/good/*.arvo stdlib/*.arvo ; do
    ./target/debug/arvo $f > $f.log 2>&1
    if [ $? -ne 0 ] || grep -q ERROR $f.log ; then
        echo test failed: $f
        GOOD=1
    else
        rm -f $f.log
    fi
done

if [ $GOOD -eq 0 ] ; then echo all Rust tests passed!; fi

exit $GOOD
