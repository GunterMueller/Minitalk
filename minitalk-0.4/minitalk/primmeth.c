/*
 * primmeth.c -- primitive methods
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "common.h"
#include "utils.h"
#include "machine.h"
#include "struct.h"
#include "memory.h"
#include "virtproc.h"
#include "primmeth.h"
#include "compiler.h"

#include "getline.h"


#define RCVR		getPointer(machine.currentStack, \
				   machine.sp - numArgs - 1)

#define ARG(x)		getPointer(machine.currentStack, \
				   machine.sp - numArgs - 1 + (x))

#define FAIL()		{ return false; }

#define SUCCEED(o)	{ ObjPtr retObj = (o); \
			  machine.sp -= numArgs + 1; \
			  push(retObj); \
			  return true; }


#define MAX_FILES	20

static FILE *fileArray[MAX_FILES] = {
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL
};


static char inputBuffer[MAX_CHUNKSIZE];


static Bool illPrim(int numArgs) {
  error("illegal primitive encountered");
  FAIL();
}


static Bool prim000(int numArgs) {
  /* Object >> halt: */
  printf("\n");
  printString(ARG(1));
  printf("\n");
  debugProcessor = true;
  SUCCEED(RCVR);
}


static Bool prim001(int numArgs) {
  /* Object >> class */
  SUCCEED(getClass(RCVR));
}


static Bool prim002(int numArgs) {
  Word size;
  char prompt[LINE_SIZE];
  char *line;

  /* String class >> inputWithPrompt: */
  /* fgets(inputBuffer, MAX_CHUNKSIZE, stdin); */
  size = getSize(ARG(1));
  memcpy(prompt, getBytes(ARG(1)), size);
  prompt[size] = '\0';
  line = gl_getline(prompt);
  gl_histadd(line);
  /* SUCCEED(newString(inputBuffer)); */
  SUCCEED(newString(line));
}


static Bool prim003(int numArgs) {
  /* String >> output */
  printString(RCVR);
  fflush(stdout);
  SUCCEED(RCVR);
}


static Bool prim004(int numArgs) {
  Word classchar;

  /* Behavior >> basicNew */
  classchar = smallIntegerValue(getPointer(RCVR, CHARACTERISTIC_IN_CLASS));
  SUCCEED(allocateObject(RCVR,
			 classchar & CLASSCHAR_NUMBERMASK,
			 classchar & CLASSCHAR_HASPOINTERS));
}


static Bool prim005(int numArgs) {
  Word classchar;

  /* Behavior >> basicNew: */
  classchar = smallIntegerValue(getPointer(RCVR, CHARACTERISTIC_IN_CLASS));
  SUCCEED(allocateObject(RCVR,
	    (classchar & CLASSCHAR_NUMBERMASK) + smallIntegerValue(ARG(1)),
	    classchar & CLASSCHAR_HASPOINTERS));
}


static Bool prim006(int numArgs) {
  ObjPtr object;
  ObjPtr stack;
  int i;

  /* BlockContext >> value */
  /* BlockContext >> value: */
  /* BlockContext >> value:value: */
  /* BlockContext >> value:value:value: */
  if (numArgs !=
      smallIntegerValue(getPointer(RCVR, NUMBERARGS_IN_BLOCKCONTEXT))) {
    FAIL();
  }
  setPointer(RCVR,
	     CALLER_IN_BLOCKCONTEXT,
	     machine.currentActiveContext);
  setPointer(RCVR,
	     INSTPTR_IN_BLOCKCONTEXT,
	     getPointer(RCVR, INITIALIP_IN_BLOCKCONTEXT));
  object = newSmallInteger(numArgs);
  setPointer(RCVR,
	     STACKPTR_IN_BLOCKCONTEXT,
	     object);
  stack = getPointer(RCVR, STACK_IN_BLOCKCONTEXT);
  for (i = 0; i < numArgs; i++) {
    setPointer(stack, i, ARG(numArgs - i));
  }
  object = RCVR;
  machine.sp -= numArgs + 1;
  storeContextRegisters();
  machine.currentActiveContext = object;
  fetchContextRegisters();
  return true;
}


static Bool prim007(int numArgs) {
  Word size;

  /* Compiler >> compile:in:lastValueNeeded: */
  size = getSize(ARG(1));
  strncpy(inputBuffer, getBytes(ARG(1)), size);
  inputBuffer[size] = '\0';
  compile(inputBuffer, ARG(2), ARG(3) == machine.true);
  SUCCEED(machine.compilerAssociation);
}


static Bool prim008(int numArgs) {
  Word size1, size2;
  ObjPtr object;
  Byte *src, *dst;

  /* String >> , */
  size1 = getSize(RCVR);
  size2 = getSize(ARG(1));
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size1 + size2,
			  false);
  src = getBytes(RCVR);
  dst = getBytes(object);
  strncpy(dst, src, size1);
  src = getBytes(ARG(1));
  dst += size1;
  strncpy(dst, src, size2);
  SUCCEED(object);
}


static Bool prim009(int numArgs) {
  /* Object >> == */
  if (RCVR == ARG(1)) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim010(int numArgs) {
  char numBuffer[20];
  Word size;
  ObjPtr object;

  /* SmallInteger >> printString */
  sprintf(numBuffer, "%ld", smallIntegerValue(RCVR));
  size = strlen(numBuffer);
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  false);
  strncpy(getBytes(object), numBuffer, size);
  SUCCEED(object);
}


static Bool prim011(int numArgs) {
  /* Character >> output */
  printf("%c", getByte(RCVR, 0));
  SUCCEED(RCVR);
}


static Bool prim012(int numArgs) {
  char commandBuffer[80];
  Word size;

  /* Editor class >> editFile: */
  strcpy(commandBuffer, "joe ");
  size = getSize(ARG(1));
  strncat(commandBuffer, getBytes(ARG(1)), size);
  system(commandBuffer);
  SUCCEED(RCVR);
}


static Bool prim013(int numArgs) {
  char commandBuffer[80];
  Word size;

  /* File class >> delete: */
  strcpy(commandBuffer, "rm ");
  size = getSize(ARG(1));
  strncat(commandBuffer, getBytes(ARG(1)), size);
  system(commandBuffer);
  SUCCEED(RCVR);
}


static Bool prim014(int numArgs) {
  int handle;
  Word size;
  char nameBuffer[20], modeBuffer[20];
  FILE *file;

  /* File class >> openFile:withMode: */
  for (handle = 0; handle < MAX_FILES; handle++) {
    if (fileArray[handle] == NULL) {
      break;
    }
  }
  if (handle == MAX_FILES) {
    FAIL();
  }
  size = getSize(ARG(1));
  strncpy(nameBuffer, getBytes(ARG(1)), size);
  nameBuffer[size] = '\0';
  size = getSize(ARG(2));
  strncpy(modeBuffer, getBytes(ARG(2)), size);
  modeBuffer[size] = '\0';
  file = fopen(nameBuffer, modeBuffer);
  if (file == NULL) {
    FAIL();
  }
  fileArray[handle] = file;
  SUCCEED(newSmallInteger(handle));
}


static Bool prim015(int numArgs) {
  int handle;

  /* File >> close */
  handle = smallIntegerValue(getPointer(RCVR, 0));
  if (handle < 0 ||
      handle >= MAX_FILES ||
      fileArray[handle] == NULL) {
    FAIL();
  }
  fclose(fileArray[handle]);
  fileArray[handle] = NULL;
  SUCCEED(RCVR);
}


static Bool prim016(int numArgs) {
  int handle;
  Word size;
  ObjPtr object;

  /* File >> readAt:length: */
  handle = smallIntegerValue(getPointer(RCVR, 0));
  if (handle < 0 ||
      handle >= MAX_FILES ||
      fileArray[handle] == NULL) {
    FAIL();
  }
  fseek(fileArray[handle], smallIntegerValue(ARG(1)), SEEK_SET);
  size = smallIntegerValue(ARG(2));
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  false);
  if (fread(getBytes(object), 1, size, fileArray[handle]) != size) {
    FAIL();
  }
  SUCCEED(object);
}


static Bool prim017(int numArgs) {
  int handle;
  Word size;

  /* File >> write: */
  handle = smallIntegerValue(getPointer(RCVR, 0));
  if (handle < 0 ||
      handle >= MAX_FILES ||
      fileArray[handle] == NULL) {
    FAIL();
  }
  size = getSize(ARG(1));
  if (fwrite(getBytes(ARG(1)), 1, size, fileArray[handle]) != size) {
    FAIL();
  }
  SUCCEED(RCVR);
}


static Bool prim018(int numArgs) {
  int handle;
  Word size1, size2;

  /* File >> size */
  handle = smallIntegerValue(getPointer(RCVR, 0));
  if (handle < 0 ||
      handle >= MAX_FILES ||
      fileArray[handle] == NULL) {
    FAIL();
  }
  size1 = ftell(fileArray[handle]);
  fseek(fileArray[handle], 0, SEEK_END);
  size2 = ftell(fileArray[handle]);
  fseek(fileArray[handle], size1, SEEK_SET);
  SUCCEED(newSmallInteger(size2));
}


static Bool prim019(int numArgs) {
  /* String >> size */
  SUCCEED(newSmallInteger(getSize(RCVR)));
}


static Bool prim020(int numArgs) {
  int handle;

  /* Driver >> stop */
  for (handle = 0; handle < MAX_FILES; handle++) {
    if (fileArray[handle] != NULL) {
      fclose(fileArray[handle]);
      fileArray[handle] = NULL;
    }
  }
  run = false;
  SUCCEED(RCVR);
}


static Bool prim021(int numArgs) {
  Word size1, size2;

  /* String >> = */
  size1 = getSize(RCVR);
  size2 = getSize(ARG(1));
  if (size1 == size2 &&
      strncmp(getBytes(RCVR), getBytes(ARG(1)), size1) == 0) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim022(int numArgs) {
  /* Object >> basicInspect */
  showObject(NULL, RCVR);
  SUCCEED(RCVR);
}


static Bool prim023(int numArgs) {
  int c;

  /* Object >> confirm: */
  while (1) {
    printString(ARG(1));
    printf(" (y/n) ");
    fflush(stdout);
    c = getchar();
    putchar('\n');
    if (c == 'y' || c == 'n') {
      break;
    }
    printf("Sorry, only 'y' or 'n' can be accepted here.\n");
  }
  if (c == 'y') {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim024(int numArgs) {
  Word size;

  /* Object >> totalSize */
  size = getSize(RCVR);
  SUCCEED(newSmallInteger(size));
}


static Bool prim025(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> bitAnd: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 & iop2));
}


static Bool prim026(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> divSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  if (iop2 == 0) {
    FAIL();
  }
  if (iop1 % iop2 != 0) {
    FAIL();
  }
  SUCCEED(newSmallInteger(iop1 / iop2));
}


static Bool prim027(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> addSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 + iop2));
}


static Bool prim028(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> subSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 - iop2));
}


static Bool prim029(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> mulSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 * iop2));
}


static Bool prim030(int numArgs) {
  long index, namedSize;
  Word classchar;

  /* Object >> instVarAt: */
  index = smallIntegerValue(ARG(1)) - 1;
  classchar =
    smallIntegerValue(getPointer(getClass(RCVR), CHARACTERISTIC_IN_CLASS));
  namedSize = classchar & CLASSCHAR_NUMBERMASK;
  if (index < 0 || index >= namedSize) {
    FAIL();
  }
  SUCCEED(getPointer(RCVR, index));
}


static Bool prim031(int numArgs) {
  long index, namedSize;
  Word classchar;

  /* Object >> instVarAt:put: */
  index = smallIntegerValue(ARG(1)) - 1;
  classchar =
    smallIntegerValue(getPointer(getClass(RCVR), CHARACTERISTIC_IN_CLASS));
  namedSize = classchar & CLASSCHAR_NUMBERMASK;
  if (index < 0 || index >= namedSize) {
    FAIL();
  }
  setPointer(RCVR, index, ARG(2));
  SUCCEED(ARG(2));
}


static Bool prim032(int numArgs) {
  long index, namedSize, totalSize;
  Word classchar;

  /* Object >> basicAt: */
  classchar =
    smallIntegerValue(getPointer(getClass(RCVR), CHARACTERISTIC_IN_CLASS));
  namedSize = classchar & CLASSCHAR_NUMBERMASK;
  index = namedSize + smallIntegerValue(ARG(1)) - 1;
  totalSize = getSize(RCVR);
  if (index < namedSize || index >= totalSize) {
    FAIL();
  }
  SUCCEED(getPointer(RCVR, index));
}


static Bool prim033(int numArgs) {
  long index, namedSize, totalSize;
  Word classchar;

  /* Object >> basicAt:put: */
  classchar =
    smallIntegerValue(getPointer(getClass(RCVR), CHARACTERISTIC_IN_CLASS));
  namedSize = classchar & CLASSCHAR_NUMBERMASK;
  index = namedSize + smallIntegerValue(ARG(1)) - 1;
  totalSize = getSize(RCVR);
  if (index < namedSize || index >= totalSize) {
    FAIL();
  }
  setPointer(RCVR, index, ARG(2));
  SUCCEED(ARG(2));
}


static Bool prim034(int numArgs) {
  int handle;
  Word size;
  ObjPtr object;

  /* File >> readAll */
  handle = smallIntegerValue(getPointer(RCVR, 0));
  if (handle < 0 ||
      handle >= MAX_FILES ||
      fileArray[handle] == NULL) {
    FAIL();
  }
  size = fread(inputBuffer, 1, MAX_CHUNKSIZE, fileArray[handle]);
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  false);
  strncpy(getBytes(object), inputBuffer, size);
  SUCCEED(object);
}


static Bool prim035(int numArgs) {
  long index, totalSize;

  /* String >> at: */
  index = smallIntegerValue(ARG(1)) - 1;
  totalSize = getSize(RCVR);
  if (index < 0 || index >= totalSize) {
    FAIL();
  }
  SUCCEED(newCharacter(getByte(RCVR, index)));
}


static Bool prim036(int numArgs) {
  long index, totalSize;

  /* String >> at:put: */
  index = smallIntegerValue(ARG(1)) - 1;
  totalSize = getSize(RCVR);
  if (index < 0 || index >= totalSize) {
    FAIL();
  }
  setByte(RCVR, index, ARG(2));
  SUCCEED(ARG(2));
}


static Bool prim037(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> equalSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  if (iop1 == iop2) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim038(int numArgs) {
  long iop1, iop2;

  /* SmallInteger >> lessSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  if (iop1 < iop2) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim039(int numArgs) {
  double fop1, fop2;

  /* Float >> equalFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  if (fop1 == fop2) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim040(int numArgs) {
  double fop1, fop2;

  /* Float >> lessFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  if (fop1 < fop2) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


static Bool prim041(int numArgs) {
  double fop1, fop2;

  /* Float >> divFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  if (fop2 == 0.0) {
    FAIL();
  }
  SUCCEED(newFloat(fop1 / fop2));
}


static Bool prim042(int numArgs) {
  double fop1, fop2;

  /* Float >> addFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 + fop2));
}


static Bool prim043(int numArgs) {
  double fop1, fop2;

  /* Float >> subFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 - fop2));
}


static Bool prim044(int numArgs) {
  double fop1, fop2;

  /* float >> mulFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 * fop2));
}


static Bool prim045(int numArgs) {
  char numBuffer[20];
  Word size;
  ObjPtr object;

  /* Float >> printString */
  sprintf(numBuffer, "%e", floatValue(RCVR));
  size = strlen(numBuffer);
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  false);
  strncpy(getBytes(object), numBuffer, size);
  SUCCEED(object);
}


static Bool prim046(int numArgs) {
  long iop;

  /* SmallInteger >> asFloat */
  iop = smallIntegerValue(RCVR);
  SUCCEED(newFloat(iop));
}


static Bool prim047(int numArgs) {
  ObjPtr selector, class;
  ObjPtr oldClass, oldMethod;
  int i;

  /* Object >> perform: */
  /* Object >> perform:with: */
  /* Object >> perform:with:with: */
  /* Object >> perform:with:with:with: */
  /* get the selector of the message */
  selector = getPointer(machine.currentStack,
			machine.sp - numArgs);
  /* determine the class where to start the search */
  class = getClass(getPointer(machine.currentStack,
			      machine.sp - numArgs - 1));
  /* findMethod() sets machine.newClass and machine.newMethod */
  /* save old values of these in case primitive fails later */
  oldClass = machine.newClass;
  oldMethod = machine.newMethod;
  findMethod(class, selector);
  if (debugProcessor) {
    showWhere(class, machine.newClass, selector);
  }
  /* check number of arguments */
  if (numArgs - 1 !=
      smallIntegerValue(getPointer(machine.newMethod,
				   NUMBERARGS_IN_COMPILEDMETHOD))) {
    machine.newClass = oldClass;
    machine.newMethod = oldMethod;
    FAIL();
  }
  for (i = numArgs - 1; i > 0; i--) {
    setPointer(machine.currentStack,
	       machine.sp - i - 1,
	       getPointer(machine.currentStack, machine.sp - i));
  }
  pop();
  executeNewMethod(numArgs - 1);
  return true;
}


static Bool prim048(int numArgs) {
  /* Object >> become: */
  swapPointers(RCVR, ARG(1));
  SUCCEED(RCVR);
}


static Bool prim049(int numArgs) {
  Word hashval, aux;
  char *cp;
  Word size;

  /* String >> hash */
  hashval = 0;
  cp = getBytes(RCVR);
  size = getSize(RCVR);
  while (size--) {
    hashval <<= 4;
    hashval += *cp++;
    aux = hashval & 0xF0000000;
    if (aux != 0) {
      hashval ^= aux >> 24;
      hashval ^= aux;
    }
  }
  SUCCEED(newSmallInteger(hashval & 0x3FFFFFFF));
}


static Bool prim050(int numArgs) {
  double fop;

  /* Float >> floor */
  fop = floatValue(RCVR);
  SUCCEED(newSmallInteger((long) floor(fop)));
}


static Bool prim051(int numArgs) {
  double fop;

  /* Float >> ceil */
  fop = floatValue(RCVR);
  SUCCEED(newSmallInteger((long) ceil(fop)));
}


static Bool prim052(int numArgs) {
  double fop;

  /* Float >> sin */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(sin(fop)));
}


static Bool prim053(int numArgs) {
  double fop;

  /* Float >> cos */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(cos(fop)));
}


static Bool prim054(int numArgs) {
  double fop;

  /* Float >> tan */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(tan(fop)));
}


static Bool prim055(int numArgs) {
  double fop;

  /* Float >> asin */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(asin(fop)));
}


static Bool prim056(int numArgs) {
  double fop;

  /* Float >> acos */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(acos(fop)));
}


static Bool prim057(int numArgs) {
  double fop;

  /* Float >> atan */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(atan(fop)));
}


static Bool prim058(int numArgs) {
  double fop;

  /* Float >> exp */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(exp(fop)));
}


static Bool prim059(int numArgs) {
  double fop;

  /* Float >> log */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(log(fop)));
}


static Bool prim060(int numArgs) {
  double fop;

  /* Float >> sqrt */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(sqrt(fop)));
}


static Bool prim061(int numArgs) {
  ObjPtr bytecodes, literals;

  /* CompiledMethod >> disassemble */
  bytecodes = getPointer(RCVR, BYTECODES_IN_COMPILEDMETHOD);
  literals = getPointer(RCVR, LITERALS_IN_COMPILEDMETHOD);
  showInstructions(bytecodes, literals);
  SUCCEED(RCVR);
}


PrimitiveMethod primitiveMethods[256] = {
  prim000, prim001, prim002, prim003, prim004, prim005, prim006, prim007,
  prim008, prim009, prim010, prim011, prim012, prim013, prim014, prim015,
  prim016, prim017, prim018, prim019, prim020, prim021, prim022, prim023,
  prim024, prim025, prim026, prim027, prim028, prim029, prim030, prim031,
  prim032, prim033, prim034, prim035, prim036, prim037, prim038, prim039,
  prim040, prim041, prim042, prim043, prim044, prim045, prim046, prim047,
  prim048, prim049, prim050, prim051, prim052, prim053, prim054, prim055,
  prim056, prim057, prim058, prim059, prim060, prim061, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim,
  illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim, illPrim
};
