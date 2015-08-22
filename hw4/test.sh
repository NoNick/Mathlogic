DIR=../logic2014/tests/HW4

for i in $(seq 1 20); do
    FILE=$DIR/correct$i.in
    if [ -f $FILE ]; then
	cp $FILE input.txt
	./Main
	echo -e "\tOutput for "$FILE
	cat output.txt
	echo ""
    fi 
done

for i in $(seq 1 20); do
    FILE=$DIR/incorrect$i.in
    if [ -f $FILE ]; then
	cp $FILE input.txt
	./Main
	echo -e "\tOutput for "$FILE
	cat output.txt
	echo ""
    fi 
done
