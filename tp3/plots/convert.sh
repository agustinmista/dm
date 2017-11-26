#!/bin/sh

for i in $(ls *.svg) 
do
  o=`echo $i | sed 's/\.[^.]*$//'`
  echo "$i --> $o.png"
  inkscape -z $i -e $o.png 
done
