#!/bin/bash


function transimage() {
    echo "transform $1.bmp to $1.hpeg"
    bmp2hpeg $1.bmp $1.hpeg
    echo "transform $1.hpeg to $1.out.bmp"
    hpeg2bmp $1.hpeg $1.out.bmp
    # rename the eventlog
    echo "rename the event log to $1.bh.eventlog (bmp -> hpeg) and $1.hb.eventlog (hpeg -> bmp)"
    mv bmp2hpeg.eventlog $1.bh.eventlog
    mv hpeg2bmp.eventlog $1.hb.eventlog
}

if [ -z "$1" ]; then
    ls -1 | grep "test[0-9]*.bmp" | awk -F. '{print "./test.sh "$1}'
else
    transimage $1
fi;
