#
# Makefile for MiniTalk virtual machine
#

BUILD = ../build

CC = gcc
CFLAGS = -Wall -g -I./getline -I../compiler -I../common
LDFLAGS = -g -L./getline -L../compiler -L../common -L/usr/X11R6/lib
LDLIBS = -lgetline -lcompiler -lminitalk -lX11 -lpthread -lm

SRCS = main.c virtproc.c primmeth.c graph.c
OBJS = $(patsubst %.c,%.o,$(SRCS))
BIN = minitalk

.PHONY:		all install clean

all:		$(BIN)

install:	$(BIN)
		mkdir -p $(BUILD)/bin
		cp $(BIN) $(BUILD)/bin

getline/libgetline.a:
		$(MAKE) -C getline

$(BIN):		$(OBJS) getline/libgetline.a
		$(CC) $(LDFLAGS) -o $(BIN) $(OBJS) $(LDLIBS)

%.o:		%.c
		$(CC) $(CFLAGS) -o $@ -c $<

depend.mak:	$(SRCS)
		$(CC) -MM -MG $(CFLAGS) $(SRCS) > depend.mak

-include depend.mak

clean:
		$(MAKE) -C getline clean
		rm -f *~ $(OBJS) $(BIN) depend.mak *.img *.src junk.*
