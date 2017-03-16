# FIT VUTBR - FLP - project1 - dka-2-mka
# Jan Kubis / xkubis13
EXECUTABLE=dka-2-mka
SOURCE=dka2mka.hs FaModule.hs
ZIPFILE=flp-fun-xkubis13.zip

default: clear
	ghc ${SOURCE} -o ${EXECUTABLE}

zip:
	zip ${ZIPFILE} ${SOURCE} README Makefile

run: default
	./${EXECUTABLE} ${arg1} ${arg2}

clear:
	rm -rf *.hi *.o ${EXECUTABLE} ${ZIPFILE}