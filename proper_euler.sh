#!/usr/bin/zsh
sizes=(1 2 4 8 20 50 100 125 250 500 1000 2000 4000)
lengths=(4000)
chunk=(0 1 2)
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
	../pycket/pycket-c  sumeuler.rkt -c $size -l $length > "trans_benchmarks/eulerchunkwu${length}x${size}"
	#task
	for c in $chunk
	do
	    for i in {1..10}
	    do
		../pycket/pycket-c  sumeuler.rkt  -c $size -t -l $length -p $c >> "trans_benchmarks/eulerchunk${length}x${size}x${c}"
	    done
	done

	#strided
	../pycket/pycket-c  sumeuler.rkt -c $size -s -l $length > "trans_benchmarks/eulerstridewu${length}x${size}"
	#task
	for i in {1..10}
	do
	    ../pycket/pycket-c  sumeuler.rkt  -c $size -t -s -l $length >> "trans_benchmarks/eulerstridetask${length}x${size}"
	done
	
    done
done
