#!/usr/bin/env zsh
ns=(42)
threshes=(15 16 17 18 21 24 27 30 33 36 39 42)


for n in $ns
do

    for thresh in $threshes
    do
	if [[ $thresh -gt $n ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c fib.rkt -o $thresh -n $n 

	#task
    echo "SIZE: ${n}"
	for i in {1..10}
	do
	    ../pycket/pycket-c fib.rkt -o $thresh -t -n $n 
	done
    done
done
