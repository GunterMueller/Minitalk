#
# Makefile for MiniTalk compiler
#

CC = gcc
CFLAGS = -Wall -g -I../common

SRCS = compiler.c tree.c scanner.c parser.c
OBJS = $(patsubst %.c,%.o,$(SRCS))
LIB = libcompiler.a

.PHONY:		all install clean

all:		$(LIB)

install:	$(LIB)

$(LIB):		$(OBJS)
		ar -rs $(LIB) $(OBJS)

%.o:		%.c
		$(CC) $(CFLAGS) -o $@ -c $<

depend.mak:	$(SRCS)
		$(CC) -MM -MG $(CFLAGS) $(SRCS) > depend.mak

-include depend.mak

clean:
		rm -f *~ $(OBJS) $(LIB) depend.mak
