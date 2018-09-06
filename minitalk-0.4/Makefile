#
# Makefile for MiniTalk project
#

VERSION = 0.4

DIRS = doc common compiler mkimage minitalk
BUILD = `pwd`/build

all:
		for i in $(DIRS) ; do $(MAKE) -C $$i install ; done

clean:
		for i in $(DIRS) ; do $(MAKE) -C $$i clean ; done
		rm -f *~
		rm -rf $(BUILD)

dist:		clean
		(cd .. ; \
		 tar -cvf minitalk-$(VERSION).tar minitalk-$(VERSION)/* ; \
		 gzip -f minitalk-$(VERSION).tar)
