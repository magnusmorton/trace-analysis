#!/usr/bin/zsh
sizes=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 333 500 1000)
dims=(100 500 1000)
# untransformed
#warmup

../pycket/pycket-c  matmulfl.rkt -c 0 > trans_benchmarks/matmulseqwu

#task
for i in {1..10}
do
    ../pycket/pycket-c  matmulfl.rkt  -c 0 -t >> trans_benchmarks/matmulseqtask
done	 

#transformed
for dim in $dims
do
    #echo $dim
    for size in $sizes
    do
	if [[ $size -gt $dim ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c  matmulfl.rkt -c $size > "trans_benchmarks/matmultranswu${dim}x${size}"
	#task
	for i in {1..10}
	do
	    ../pycket/pycket-c  matmulfl.rkt  -c $size -t >> "trans_benchmarks/matmultranstask${dim}x${size}"
	done	 
    done
done
