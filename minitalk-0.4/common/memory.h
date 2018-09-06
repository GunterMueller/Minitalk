/*
 * memory.c -- object memory
 */


#ifndef _MEMORY_H_
#define _MEMORY_H_


extern Bool debugMemory;


ObjPtr allocateObject(ObjPtr class, Word size, Bool hasPointers);

ObjPtr getClass(ObjPtr object);
void patchClass(ObjPtr object, ObjPtr class);
Word getSize(ObjPtr object);
Bool hasPointers(ObjPtr object);
Byte *getBytes(ObjPtr object);
Byte getByte(ObjPtr object, Word index);
void setByte(ObjPtr object, Word index, Byte value);
ObjPtr getPointer(ObjPtr object, Word index);
void setPointer(ObjPtr object, Word index, ObjPtr value);
ObjPtr newSmallInteger(long value);
long smallIntegerValue(ObjPtr smallIntegerObj);
ObjPtr newFloat(double value);
double floatValue(ObjPtr floatObj);
ObjPtr newCharacter(Byte c);
Byte characterValue(ObjPtr characterObj);
ObjPtr newString(char *string);
char *stringValue(ObjPtr stringObj);
ObjPtr newSymbol(char *string);

void swapPointers(ObjPtr obj1, ObjPtr obj2);

void initMemory(char *imageFileName);
void exitMemory(char *imageFileName);


#endif /* _MEMORY_H_ */
