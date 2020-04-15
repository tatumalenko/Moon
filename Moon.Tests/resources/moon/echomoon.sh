OUTPUT=`./moon $@`
echo $OUTPUT
echo $2.out
echo $OUTPUT > $2.out
