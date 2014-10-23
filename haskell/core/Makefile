all: helloworld helloworld.core boom boom.core

.PHONY: clean

clean:
	rm -f *.hi boom helloworld *.core *.hcr *.o

helloworld: helloworld.hs
	ghc -fext-core -o helloworld helloworld.hs

helloworld.core: helloworld.hs
	ghc-core --no-syntax --no-cast --no-asm helloworld.hs > helloworld.core

boom: boom.hs
	ghc -fext-core -o boom boom.hs

boom.core: boom.hs
	ghc-core --no-syntax --no-cast --no-asm boom.hs > boom.core
