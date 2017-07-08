#!/bin/sh

if [ $# -lt 1 ]
then
    echo "save"
    echo "to save the file 'problems-algorithms/main.c' in a new name"
    echo "usage: ./save.sh <filename>"
    exit
fi

cp problems-algorithms/main.c "$*.c"


