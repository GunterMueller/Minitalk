#
# Makefile for MiniTalk library
#

CC = gcc
CFLAGS = -Wall -g -I../common

SRCS = utils.c machine.c memory.c
OBJS = $(patsubst %.c,%.o,$(SRCS))
LIB = libminitalk.a

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
