EXECUTABLE="dka-2-mka"
SOURCE="dka2mka.hs FaModule.hs"
ZIPFILE="flp-fun-xkubis13.zip"

if [ $1 == "zip" ]; then
	echo "generating zip file..."
	zip $ZIPFILE $SOURCE "README"
	exit
fi

if [ -f $EXECUTABLE ]; then
	rm $EXECUTABLE
fi

echo "-- COMPILATION ----------------------------------------"
ghc $SOURCE -o $EXECUTABLE
echo "-------------------------------------------------------"

if [ -f $EXECUTABLE ]; then
	echo "BASH_SAMPLE_INPUT" | ./$EXECUTABLE $1 $2 $3 $4
fi