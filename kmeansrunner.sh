#!/usr/bin/zsh
workers=(1 2 4 6 8 10 12 16 32 50 64 100 125 250 500)
    # untransformed
    #warmup

    #transformed
    #echo $dim
for size in $workers
do
    #warmup
    ../pycket/pycket-c  synthkmeans.rkt -f /scratch1/magnus/foo.txt -k 5 -c $size m > "trans_benchmarks/kmeans${dim}x${size}wu"
    #task
    for i in {1..10}
    do
	echo "."
	
	../pycket/pycket-c synthkmeans.rkt -f /scratch1/magnus/foo.txt -k 5 -t -c $size m > "trans_benchmarks/kmeans${dim}x${size}task"
    done	 
done
