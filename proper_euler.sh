#!/usr/bin/zsh
sizes=(1 2 4 8 20 50 100 125 250 500 1000 2000 4000)
lengths=(4000)
chunk=(0 1 2)
# untransformed
#warmup

#transformed
for length in $lengths
do
    for size in $sizes
    do
	if [[ $size -gt $length ]]; then
	    break
	fi

	#task
    echo "SIZE: ${size}"
	for i in {1..10}
	do
	    ../pycket/pycket-c  sumeuler.rkt  -c $size -t -s -l $length 
	done
	
    done
done
