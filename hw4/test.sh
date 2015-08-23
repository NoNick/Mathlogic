if [ -z "$1" ]; then
    DIR=../logic2014/tests/HW4/*
else DIR=$1/*
fi

ghc $SRC

for FILE in $DIR; do
	cp $FILE input.txt
	$EXE
	if [ -z "$2" ]; then
	    echo -e "\tOutput for "$FILE
	    cat output.txt
	    echo ""
	else
	    OUT=$(basename $FILE)
	    cp output.txt $2/$OUT
	    echo "Saved in $2/$OUT"
	fi
done
