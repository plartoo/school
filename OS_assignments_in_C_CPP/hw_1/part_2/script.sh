#!/bin/bash
count=0
for i in {0..10000..2}
  do
	count=`expr $count + 1`
     #echo "Welcome $i times"
 done
echo "DONE running script"
