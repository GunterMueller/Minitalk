#
# Makefile for MiniTalk image generator
#

BUILD = ../build

CC = gcc
CFLAGS = -Wall -g -I../compiler -I../common
LDFLAGS = -g -L../compiler -L../common
LDLIBS = -lcompiler -lminitalk -lm

SRCS = mkimage.c
OBJS = $(patsubst %.c,%.o,$(SRCS))
BIN = mkimage

.PHONY:		all install clean

all:		$(BIN)

install:	$(BIN)
		./$(BIN) ../classes/classes.lst
		mkdir -p $(BUILD)/lib
		cp minitalk-std.img $(BUILD)/lib
		cp minitalk-std.src $(BUILD)/lib

$(BIN):		$(OBJS)
		$(CC) $(LDFLAGS) -o $(BIN) $(OBJS) $(LDLIBS)

%.o:		%.c
		$(CC) $(CFLAGS) -o $@ -c $<

depend.mak:	$(SRCS)
		$(CC) -MM -MG $(CFLAGS) $(SRCS) > depend.mak

-include depend.mak

clean:
		rm -f *~ $(OBJS) $(BIN) depend.mak *.img *.src
