main.o: main.c ../common/common.h ../common/utils.h ../common/memory.h \
 virtproc.h ../compiler/compiler.h ../compiler/tree.h \
 ../compiler/scanner.h ../compiler/parser.h
virtproc.o: virtproc.c ../common/common.h ../common/utils.h \
 ../common/machine.h ../common/struct.h ../common/memory.h \
 ../common/opcodes.h virtproc.h primmeth.h getline/getline.h
primmeth.o: primmeth.c ../common/common.h ../common/utils.h \
 ../common/machine.h ../common/struct.h ../common/memory.h virtproc.h \
 primmeth.h ../compiler/compiler.h getline/getline.h graph.h
graph.o: graph.c ../common/common.h ../common/utils.h graph.h splash
