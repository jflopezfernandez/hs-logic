
CC := ghc

CFLAGS := -O

all: logic.exe

logic.exe: main.o
	$(CC) $(CFLAGS) -o $@ $^

main.o: main.hs
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -rf *.o *.exe
