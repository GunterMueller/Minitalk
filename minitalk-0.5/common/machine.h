/*
 * machine.h -- virtual machine state data
 */


#ifndef _MACHINE_H_
#define _MACHINE_H_


#define SIGNATURE_1	0x37A2F90B
#define SIGNATURE_2	0x1E84C56D


typedef struct {
  /* image file signature */
  Word signature_1;		/* must be SIGNATURE_1 */
  Word signature_2;		/* must be SIGNATURE_2 */
  /* image file version number */
  int majorVersion;		/* same as main program's major version */
  int minorVersion;		/* main's minor version, is not checked */
  /* image file structure */
  long memoryStart;		/* byte offset of object memory in file */
  long memorySize;		/* total size of object memory in bytes */
  /* known objects */
  ObjPtr nil;
  ObjPtr false;
  ObjPtr true;
  ObjPtr character[256];
  ObjPtr UndefinedObject;
  ObjPtr False;
  ObjPtr True;
  ObjPtr Character;
  ObjPtr SmallInteger;
  ObjPtr Float;
  ObjPtr LinkedObject;
  ObjPtr Association;
  ObjPtr Dictionary;
  ObjPtr Array;
  ObjPtr ByteArray;
  ObjPtr String;
  ObjPtr Symbol;
  ObjPtr CompiledMethod;
  ObjPtr BlockContext;
  ObjPtr MethodContext;
  ObjPtr Class;
  ObjPtr Metaclass;
  ObjPtr TheSymbols;
  ObjPtr MiniTalk;
  /* machine registers which hold objects */
  ObjPtr currentActiveContext;
  ObjPtr currentHomeContext;
  ObjPtr currentSender;
  ObjPtr currentCaller;
  ObjPtr currentClass;
  ObjPtr currentMethod;
  ObjPtr currentSelector;
  ObjPtr currentBytecodes;
  ObjPtr currentLiterals;
  ObjPtr currentReceiver;
  ObjPtr currentTemporaries;
  ObjPtr currentStack;
  ObjPtr newClass;
  ObjPtr newMethod;
  ObjPtr newContext;
  /* machine registers which hold non-objects */
  Word ip;
  Word sp;
  /* registers used by compiler (C version only) */
  ObjPtr compilerClass;
  ObjPtr compilerCode;
  ObjPtr compilerLiteral;
  ObjPtr compilerLiterals;
  ObjPtr compilerMethod;
  ObjPtr compilerAssociation;
} Machine;


extern Machine machine;		/* an instance of the virtual machine */


#endif /* _MACHINE_H_ */
