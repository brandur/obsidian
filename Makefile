cgi := obsidian.cgi
lhss := $(shell find . -name "*.hs" -print)
date := $(shell date +%Y-%m-%d)
DIST := obsidian_$(date)

all : $(cgi)

obsidian.cgi : $(lhss)
	ghc -Wall -threaded -package fastcgi --make -O2 -o obsidian.cgi Obsidian.hs 

clean :
	find . -iregex ".*\.\(cgi\|out\|o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm

start : $(cgi)
	/etc/init.d/brandur-haskell-fcgi start

stop : 
	/etc/init.d/brandur-haskell-fcgi stop
