#!/usr/bin/zsh
sizes=(1 2 4 8 16 32 64 128 256 512 1024 2048 4096)
lengths=(1000 2000 4000)
chunks={0..9}
# untransformed
#warmup

#transformed
for length in $lengths
do
    echo "warmup"
    ../pycket/pycket-c  sumeuler.rkt -c 0 -l $length> trans_benchmarks/eulerseqwu

    #task
    echo "task"
    for i in {1..10}
    do
	../pycket/pycket-c  sumeuler.rkt  -c 0 -t -l $length >> trans_benchmarks/eulerseqtask
    done	 

    #echo $length
    for size in $sizes
    do
	if [[ $size -gt $length ]]; then
	    break
	fi
	#warmup
	../pycket/pycket-c  sumeuler.rkt -c $size -l $length > "trans_benchmarks/eulerirrwu${length}x${size}"
	#task
	for i in {1..10}
	do
	    for chunk in $chunks
	    do
		../pycket/pycket-c  sumeuler.rkt  -c $size -t -l $length -p $chunk >> trans_benchmarks/eulerirrtask${length}x${size}x${chunk}
	    done
	done
	
    done
done
