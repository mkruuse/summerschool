COMP=cray

ifeq ($(COMP),cray)
CC=cc
CCFLAGS=-O3 -I/appl/opt/libpng/include
LIBS=-L/appl/opt/libpng/lib -lpng -lz
endif

ifeq ($(COMP),gnu)
CC=mpicc
CCFLAGS=-O3 -Wall
LIBS=-lpng
endif

EXE=dynamic_array
OBJS=pngwriter.o dynamic_array.o

all: $(EXE)

pngwriter.o: pngwriter.c pngwriter.h
dynamic_array.o: dynamic_array.c

$(EXE): $(OBJS)
	$(CC) $(CCFLAGS) $(OBJS) -o $@ $(LIBS)

%.o: %.c
	$(CC) $(CCFLAGS) -c $< -o $@

clean:
	-/bin/rm -f $(EXE) a.out *.o *.png
