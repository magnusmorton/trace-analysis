#!/usr/bin/zsh
ns=(35 36)
threshes=(1 2 4 6 8 10 15 17 30 35 36)


for n in $ns
do
    #untransformed warmup
    ../pycket/pycket-c fib.rkt -th 0 -n $n > trans_benchmarks/fibseqwu${n}

    #task
    for i in {1..10}
    do
	../pycket/pycket-c fib.rkt -th 0 -t -n $n >> trans_benchmarks/fibseqtask${n}
    done

    for thresh in $threshes
    do
	if [[ $thresh -gt $n]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c fib.rkt -th $thresh -n $n > trans_benchmarks/fibtranswu${n}x${thresh}

	#task
	for i in {1..10}
	do
	    ../pycket/pycket-c fib.rkt -th $thresh -t -n $n > trans_benchmarks/fibtranstask${n}x${thresh}
	done
    done
done
