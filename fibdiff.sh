#!/usr/bin/env zsh
ns=(30  36  42)
threshes=( 15 16 17 18 21 24 27 30 33 36 39 42)


DIR=trans_benchmarks
for n in $ns
do
    echo "seq ${n}"
    #untransformed warmup
    python differ.py ${DIR}/fibseqwu${n} ${DIR}/fibseqtask${n}


    for thresh in $threshes
    do
	if [[ $thresh -gt $n ]]; then
	    break
	fi
	echo "${n}x${thresh}"
	python differ.py ${DIR}/fibtranswu${n}x${thresh} ${DIR}/fibtranstask${n}x${thresh}
    done
done
