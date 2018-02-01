#!/usr/bin/zsh
sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000)
dims=(1000)


for dim in $dims
do

    #transformed
    #echo $dim
    for size in $sizes
    do
	if [[ $size -gt $dim ]]; then
	    break
	fi
    echo "SIZE: ${size}"
	for i in {1..10}
	do
	    echo "."
	    ../pycket/pycket-c  matmulfl.rkt  -c $size -t -m $dim 
	done	 
    done
done
