########### MakeFile #############
HC=ghc
OBJECTS=Main.o SimpleJSON.o
all:simple
SimpleJSON.o:SimpleJSON.hs
	$(HC) -c SimpleJSON.hs
Main.o:Main.hs SimpleJSON.o
	$(HC) -c Main.hs
simple:$(OBJECTS)
	$(HC) -o simple $(OBJECTS)
clean:
	rm simple *.hi *.o
