app := obsidian
hss := $(shell find . -name "*.hs" -print)

all : $(app)

obsidian : $(hss)
	ghc -Wall -threaded --make -O2 -o $(app) Obsidian.hs 

clean :
	rm $(app)
	find . -iregex ".*\.\(bin\|cgi\|out\|o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

#test : $(hss)
#	ghc -Wall --make -o test.bin Obsidian/Test/App.hs

#start : 
#	/usr/bin/cgi-fcgi -start -connect localhost:9001 $(cgi)

#stop : 
#	kill $(shell pidof $(cgi))

