/*
 * virtproc.h -- virtual processor
 */


#ifndef _VIRTPROC_H_
#define _VIRTPROC_H_


extern Bool debugProcessor;
extern Bool run;

void printString(ObjPtr stringObj);
void showObject(char *name, ObjPtr object);
void showInstructions(ObjPtr bytecodes, ObjPtr literals);
void showWhere(ObjPtr class1, ObjPtr class2, ObjPtr selector);

void push(ObjPtr object);
ObjPtr pop(void);
void storeContextRegisters(void);
void fetchContextRegisters(void);

void findMethod(ObjPtr initialClass, ObjPtr selector);
void executeNewMethod(int numArgs);
void runProcessor(void);


#endif /* _VIRTPROC_H_ */
