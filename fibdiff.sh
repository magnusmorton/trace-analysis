#!/usr/bin/env zsh
ns=(35 36)
threshes=(1 2 4 6 8 10 15 17 30 35 36)

DIR=trans_benchmarks
for n in $ns
do
    echo "seq ${n}"
    #untransformed warmup
    python differ.py ${DIR}/fibseqwu${n} ${DIR}/eulerseqtask${n}


    for thresh in $threshes
    do
	if [[ $thresh -gt $n ]]; then
	    break
	fi
	echo "${n}x${size}"
	python differ.py ${DIR}/fibtranswu${n}x${thresh} ${DIR}/eulerseqtask${n}x${thresh}
	done
    done
done
