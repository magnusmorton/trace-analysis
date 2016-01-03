#!/usr/bin/env zsh
ns=(42)
threshes=(15 16 17 18 21 24 27 30 33 36 39 42)


for n in $ns
do
    #untransformed warmup
    ../pycket/pycket-c fib.rkt -o 0 -n $n > trans_benchmarks/fibseqwu${n}

    #task
    for i in {1..10}
    do
	../pycket/pycket-c fib.rkt -o 0 -t -n $n >> trans_benchmarks/fibseqtask${n}
    done

    for thresh in $threshes
    do
	if [[ $thresh -gt $n ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c fib.rkt -o $thresh -n $n > trans_benchmarks/fibtranswu${n}x${thresh}

	#task
	for i in {1..10}
	do
	    ../pycket/pycket-c fib.rkt -o $thresh -t -n $n >> trans_benchmarks/fibtranstask${n}x${thresh}
	done
    done
done
