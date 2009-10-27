cgi := obsidian.cgi
hss := $(shell find . -name "*.hs" -print)

all : $(cgi)

obsidian.cgi : $(hss)
	ghc -Wall -threaded -package fastcgi --make -O2 -o obsidian.cgi Obsidian.hs 

test : $(hss)
	ghc -Wall --make -o test.bin Obsidian/Test/App.hs

clean :
	find . -iregex ".*\.\(bin\|cgi\|out\|o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

start : $(cgi)
	/etc/init.d/brandur-haskell-fcgi start

stop : 
	/etc/init.d/brandur-haskell-fcgi stop
