EXECUTABLE="dka2mka"
SOURCE="dka2mka.hs FaModule.hs"

if [ -f $EXECUTABLE ]; then
	rm $EXECUTABLE
fi

echo "-- COMPILATION ----------------------------------------"
ghc $SOURCE -o $EXECUTABLE
echo "-------------------------------------------------------"

if [ -f $EXECUTABLE ]; then
	echo "BASH_SAMPLE_INPUT" | ./$EXECUTABLE $1 $2 $3 $4
fi