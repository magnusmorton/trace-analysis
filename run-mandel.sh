#!/usr/bin/env zsh
ns=( 1000 2000)
sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000 2000)

for n in $ns
do
    #untransformed warmup
    ../pycket/pycket-c mandel.rkt -o 0 -n $n > trans_benchmarks/mandelseqwu${n}

    #task
    for i in {1..10}
    do
	../pycket/pycket-c mandel.rkt -o 0 -t -n $n >> trans_benchmarks/mandelseqtask${n}
    done

    for size in $sizes
    do
	if [[ $size -gt $n ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c mandel.rkt -o $size -n $n > trans_benchmarks/mandeltranswu${n}x${size}

	#task
	for i in {1..10}
	do
	    ../pycket/pycket-c mandel.rkt -o $size -t -n $n >> trans_benchmarks/mandeltranstask${n}x${size}
	done
    done
done
