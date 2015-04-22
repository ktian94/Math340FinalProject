#!/bin/bash
while read line 
do 
	echo "$(tail -n +2 $line)" > $line	 
done < csvMap.txt 
