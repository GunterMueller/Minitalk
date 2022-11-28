compiler.o: compiler.c ../common/common.h ../common/utils.h \
 ../common/machine.h ../common/struct.h ../common/memory.h compiler.h \
 tree.h scanner.h parser.h ../common/opcodes.h
tree.o: tree.c ../common/common.h ../common/utils.h ../common/machine.h \
 ../common/struct.h ../common/memory.h tree.h
scanner.o: scanner.c ../common/common.h ../common/utils.h scanner.h
parser.o: parser.c ../common/common.h tree.h scanner.h parser.h
