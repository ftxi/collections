#!/bin/sh

if [ $# -gt 0 ]
then
    echo "daily-update"
    echo "usage: ./daily-update.sh"
    exit
fi

git status | less
echo
echo "proceed change? [y/n]"

read choice

if (( choice == "y" ))
then
    current=`date +%y/%m/%d`
    if (( `cat .last-update` == current ))
    then
	current="$(current)'"
    fi
    echo $current > .last-update
    git add *
    git commit -m "daily-update-$current"
    git push origin master
    echo "done."
elif (( choice == "n" ))
then
    echo "done."
else
    echo "unknown choice $choice"
fi
