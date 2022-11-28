#
# Makefile for MiniTalk system
#

CC = gcc
CCFLAGS = -DDOS_EXTENDED -c

LD = gcc
LDFLAGS = -DDOS_EXTENDED

RM = -rm
CP = -cp

BINDIR = ../bin

OBJS1 = minitalk.o memory.o intrprtr.o primmeth.o scanner.o tree.o parser.o compiler.o
OBJS2 = mkimage.o memory.o scanner.o tree.o parser.o compiler.o

all:		minitalk image

image:		mkimage ../cls/stdclss.txt
		$(RM) *.img
		$(RM) *.mt
		mkimage
		$(CP) minitalk $(BINDIR)
		$(CP) mkimage $(BINDIR)
		$(CP) original.img $(BINDIR)
		$(CP) original.mt $(BINDIR)

minitalk:	$(OBJS1)
		$(LD) $(LDFLAGS) -o minitalk $(OBJS1) -lm

mkimage:	$(OBJS2)
		$(LD) $(LDFLAGS) -o mkimage $(OBJS2) -lm

minitalk.o:	minitalk.h minitalk.c
		$(CC) $(CCFLAGS) minitalk.c

mkimage.o:	minitalk.h mkimage.c
		$(CC) $(CCFLAGS) mkimage.c

memory.o:	minitalk.h memory.c
		$(CC) $(CCFLAGS) memory.c

intrprtr.o:	minitalk.h intrprtr.c
		$(CC) $(CCFLAGS) intrprtr.c

primmeth.o:	minitalk.h primmeth.c
		$(CC) $(CCFLAGS) primmeth.c

scanner.o:	minitalk.h scanner.c
		$(CC) $(CCFLAGS) scanner.c

tree.o:		minitalk.h tree.c
		$(CC) $(CCFLAGS) tree.c

parser.o:	minitalk.h parser.c
		$(CC) $(CCFLAGS) parser.c

compiler.o:	minitalk.h compiler.c
		$(CC) $(CCFLAGS) compiler.c

clean:
		$(RM) *.bak
		$(RM) *.obj
		$(RM) *.o

clobber:	clean
		$(RM) mkimage
		$(RM) minitalk
		$(RM) *.exe
		$(RM) *.img
		$(RM) *.mt
