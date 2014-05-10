SRC = Data/Board.hs \
      Data/WayTree.hs \
      Main.hs

all: Main

Main: pathfinder.cabal $(SRC)
	cabal build

clean:
	rm -f $(SRC:%.hs=%.hi)
	rm -f $(SRC:%.hs=%.o)
	rm -f Main
.PHONY:clean

