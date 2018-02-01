#!/usr/bin/env zsh
ns=( 10000)
sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000 2000 4000 8000 10000)
chunk=(0 1 2)
for n in $ns
do

    for size in $sizes
    do
	if [[ $size -gt $n ]]; then
	    break
	fi
	#warmup

	#task
    echo "SIZE: ${size}"
	for i in {1..10}
	do
	    for c in $chunk
	    do
		echo "chunk: ${c}"
		../pycket/pycket-c mandel.rkt -c $size -t -m $n -s $c 
	    done
	done
    done
done
