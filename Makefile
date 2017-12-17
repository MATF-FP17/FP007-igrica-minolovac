CC	= ghc
FLAGS	= -dynamic
PROGRAM = minolovac

$(PROGRAM): ms_main.hs
	$(CC) $(FLAGS) $< -o $@

.PHONY: clean

clean:
	rm -rf *.hi *.o $(PROGRAM)
