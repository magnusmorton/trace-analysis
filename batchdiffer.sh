sizes=(1 10 20 50 83 100 125 200 250 333 500 1000)

DIR=trans_benchmarks
# untransformed

echo "sequential"
python  differ.py ${DIR}/matmulseqwu  ${DIR}/matmulseqtask  

#transformed
echo "transformed"
for size in ${sizes[*]}
do
    echo ""
    echo $size
    python  differ.py ${DIR}/matmultranswu${size}  ${DIR}/matmultranstask${size}
done
