#!/usr/bin/zsh
sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000)
dims=(1000)


for dim in $dims
do
    # untransformed
    #warmup

    ../pycket/pycket-c  matmulfl.rkt -c 0 -m $dim > trans_benchmarks/matmulseqwu${dim}

    #task
    for i in {1..10}
    do
	../pycket/pycket-c  matmulfl.rkt  -c 0 -t -m $dim >> trans_benchmarks/matmulseqtask${dim}
    done	 

    #transformed
    #echo $dim
    for size in $sizes
    do
	if [[ $size -gt $dim ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c  matmulfl.rkt -c $size -m $dim > "trans_benchmarks/matmultranswu${dim}x${size}"
	#task
	for i in {1..10}
	do
	    echo "."
	    ../pycket/pycket-c  matmulfl.rkt  -c $size -t -m $dim >> "trans_benchmarks/matmultranstask${dim}x${size}"
	done	 
    done
done
