
build:
	ghc --make src/Main.hs -isrc -odir obj -hidir obj -tmpdir tmp -o bin/app

clean:
	rm obj/*.o obj/*.hi bin/* tmp/*

rebuild: clean build

run:
	./bin/app

all: build run
