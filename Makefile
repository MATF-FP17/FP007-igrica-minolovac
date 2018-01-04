CC	= ghc
FLAGS	= -dynamic
PROGRAM = minolovac
MODULES = Main.hs Random.hs Mines.hs Game.hs Drawing.hs Play.hs Window.hs

$(PROGRAM): $(MODULES)
	$(CC) $(FLAGS) $^ -o $@

.PHONY: clean

clean:
	rm -rf *.hi *.o $(PROGRAM)
