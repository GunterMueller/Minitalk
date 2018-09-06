#
# Makefile for the getline library
#

CC      = gcc
CFLAGS  = -O -DPOSIX
LDFLAGS = 

all:		libgetline.a testgl

install:

testgl:		libgetline.a testgl.o
		$(CC) $(LDFLAGS) $(CFLAGS) -o testgl testgl.o -L. -lgetline

libgetline.a:	getline.o
		ar cr libgetline.a getline.o

clean:
		rm -f *~ *.o *.a testgl
