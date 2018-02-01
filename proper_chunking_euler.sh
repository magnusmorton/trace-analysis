#!/usr/bin/zsh
sizes=(1 2 4 8 20 50 100 125 250 500 1000 2000 4000)
lengths=(4000)
chunks=(0 1 2)
# untransformed
#warmup

#transformed
for length in $lengths
do

    #echo $length
    for size in $sizes
    do
	if [[ $size -gt $length ]]; then
	    break
	fi
    echo "SIZE: ${size}"
	#task
	for i in {1..10}
	do
	    for chunk in $chunks
	    do
        echo "CHUNK: ${chunk}"
		../pycket/pycket-c  sumeuler.rkt  -c $size -t -l $length -p $chunk 
	    done
	done
	
    done
done
