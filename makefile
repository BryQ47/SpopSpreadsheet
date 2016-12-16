
build:
	ghc --make src/Main.hs -odir obj -tmpdir tmp -o bin/app

clean:
	rm obj/*.o src/*.hi bin/*

rebuild: clean build

run:
	./bin/app

all: build run
