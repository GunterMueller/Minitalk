
/*
 * PRIMMETH.C	primitive methods
 */


#include "minitalk.h"


#include <math.h>


#define RCVR		getPointer(machine.currentStack, \
				   machine.sp - numArgs - 1)

#define ARG(x)		getPointer(machine.currentStack, \
				   machine.sp - numArgs - 1 + (x))

#define FAIL()		{ return FALSE; }

#define SUCCEED(o)	{ ObjPtr retObj = (o); \
			  machine.sp -= numArgs + 1; \
			  push(retObj); \
			  return TRUE; }


#define MAX_FILES		20

FILE *fileArray[MAX_FILES] =
{
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL
};


char inputBuffer[MAX_CHUNKSIZE];


Boolean illPrim(int numArgs)
{
  numArgs++;	/* just to keep compiler happy */
  error("illegal primitive encountered");
  FAIL();
}


Boolean prim000(int numArgs)
{
  /* Object >> halt: */
  printf("\n");
  printString(ARG(1));
  printf("\n");
  debugInterpreter = TRUE;
  SUCCEED(RCVR);
}


Boolean prim001(int numArgs)
{
  /* Object >> class */
  SUCCEED(getClass(RCVR));
}


Boolean prim002(int numArgs)
{
  /* String class >> input */
  fgets(inputBuffer, MAX_CHUNKSIZE, stdin);
  SUCCEED(newString(inputBuffer));
}


Boolean prim003(int numArgs)
{
  /* String >> output */
  printString(RCVR);
  SUCCEED(RCVR);
}


Boolean prim004(int numArgs)
{
  Ulong classchar;

  /* Behavior >> basicNew */
  classchar = smallIntegerValue(getPointer(RCVR, CHARACTERISTIC_IN_CLASS));
  SUCCEED(allocateObject(RCVR,
			 classchar & CLASSCHAR_NUMBERMASK,
			 classchar & CLASSCHAR_HASPOINTERS));
}


Boolean prim005(int numArgs)
{
  Ulong classchar;

  /* Behavior >> basicNew: */
  classchar = smallIntegerValue(getPointer(RCVR, CHARACTERISTIC_IN_CLASS));
  SUCCEED(allocateObject(RCVR,
	    (classchar & CLASSCHAR_NUMBERMASK) + smallIntegerValue(ARG(1)),
	    classchar & CLASSCHAR_HASPOINTERS));
}


Boolean prim006(int numArgs)
{
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
  return TRUE;
}


Boolean prim007(int numArgs)
{
  Ulong size;

  /* Compiler >> compile:in:lastValueNeeded: */
  size = getSize(ARG(1));
  strncpy(inputBuffer, getBytes(ARG(1)), size);
  inputBuffer[size] = '\0';
  compile(inputBuffer, ARG(2), ARG(3) == machine.true);
  SUCCEED(machine.compilerAssociation);
}


Boolean prim008(int numArgs)
{
  Ulong size1, size2;
  ObjPtr object;
  Uchar *src, *dst;

  /* String >> , */
  size1 = getSize(RCVR);
  size2 = getSize(ARG(1));
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size1 + size2,
			  FALSE);
  src = getBytes(RCVR);
  dst = getBytes(object);
  strncpy(dst, src, size1);
  src = getBytes(ARG(1));
  dst += size1;
  strncpy(dst, src, size2);
  SUCCEED(object);
}


Boolean prim009(int numArgs)
{
  /* Object >> == */
  if (RCVR == ARG(1)) {
    SUCCEED(machine.true);
  } else {
    SUCCEED(machine.false);
  }
}


Boolean prim010(int numArgs)
{
  char numBuffer[20];
  Ulong size;
  ObjPtr object;

  /* SmallInteger >> printString */
  sprintf(numBuffer, "%ld", smallIntegerValue(RCVR));
  size = strlen(numBuffer);
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  FALSE);
  strncpy(getBytes(object), numBuffer, size);
  SUCCEED(object);
}


Boolean prim011(int numArgs)
{
  /* Character >> output */
  printf("%c", getByte(RCVR, 0));
  SUCCEED(RCVR);
}


Boolean prim012(int numArgs)
{
  char commandBuffer[80];
  Ulong size;

  /* Editor class >> editFile: */
  strcpy(commandBuffer, "edit ");
  size = getSize(ARG(1));
  strncat(commandBuffer, getBytes(ARG(1)), size);
  system(commandBuffer);
  SUCCEED(RCVR);
}


Boolean prim013(int numArgs)
{
  char commandBuffer[80];
  Ulong size;

  /* File class >> delete: */
  strcpy(commandBuffer, "del ");
  size = getSize(ARG(1));
  strncat(commandBuffer, getBytes(ARG(1)), size);
  system(commandBuffer);
  SUCCEED(RCVR);
}


Boolean prim014(int numArgs)
{
  int handle;
  Ulong size;
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


Boolean prim015(int numArgs)
{
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


Boolean prim016(int numArgs)
{
  int handle;
  Ulong size;
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
			  FALSE);
  if (fread(getBytes(object), 1, size, fileArray[handle]) != size) {
    FAIL();
  }
  SUCCEED(object);
}


Boolean prim017(int numArgs)
{
  int handle;
  Ulong size;

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


Boolean prim018(int numArgs)
{
  int handle;
  Ulong size1, size2;

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


Boolean prim019(int numArgs)
{
  /* String >> size */
  SUCCEED(newSmallInteger(getSize(RCVR)));
}


Boolean prim020(int numArgs)
{
  int handle;

  /* Driver >> stop */
  for (handle = 0; handle < MAX_FILES; handle++) {
    if (fileArray[handle] != NULL) {
      fclose(fileArray[handle]);
      fileArray[handle] = NULL;
    }
  }
  run = FALSE;
  SUCCEED(RCVR);
}


Boolean prim021(int numArgs)
{
  Ulong size1, size2;

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


Boolean prim022(int numArgs)
{
  /* Object >> basicInspect */
  showObject(NULL, RCVR);
  SUCCEED(RCVR);
}


Boolean prim023(int numArgs)
{
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


Boolean prim024(int numArgs)
{
  Ulong size;

  /* Object >> totalSize */
  size = getSize(RCVR);
  SUCCEED(newSmallInteger(size));
}


Boolean prim025(int numArgs)
{
  long iop1, iop2;

  /* SmallInteger >> bitAnd: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 & iop2));
}


Boolean prim026(int numArgs)
{
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


Boolean prim027(int numArgs)
{
  long iop1, iop2;

  /* SmallInteger >> addSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 + iop2));
}


Boolean prim028(int numArgs)
{
  long iop1, iop2;

  /* SmallInteger >> subSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 - iop2));
}


Boolean prim029(int numArgs)
{
  long iop1, iop2;

  /* SmallInteger >> mulSmallInteger: */
  iop1 = smallIntegerValue(RCVR);
  iop2 = smallIntegerValue(ARG(1));
  SUCCEED(newSmallInteger(iop1 * iop2));
}


Boolean prim030(int numArgs)
{
  long index, namedSize;
  Ulong classchar;

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


Boolean prim031(int numArgs)
{
  long index, namedSize;
  Ulong classchar;

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


Boolean prim032(int numArgs)
{
  long index, namedSize, totalSize;
  Ulong classchar;

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


Boolean prim033(int numArgs)
{
  long index, namedSize, totalSize;
  Ulong classchar;

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


Boolean prim034(int numArgs)
{
  int handle;
  Ulong size;
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
			  FALSE);
  strncpy(getBytes(object), inputBuffer, size);
  SUCCEED(object);
}


Boolean prim035(int numArgs)
{
  long index, totalSize;

  /* String >> at: */
  index = smallIntegerValue(ARG(1)) - 1;
  totalSize = getSize(RCVR);
  if (index < 0 || index >= totalSize) {
    FAIL();
  }
  SUCCEED(newCharacter(getByte(RCVR, index)));
}


Boolean prim036(int numArgs)
{
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


Boolean prim037(int numArgs)
{
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


Boolean prim038(int numArgs)
{
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


Boolean prim039(int numArgs)
{
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


Boolean prim040(int numArgs)
{
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


Boolean prim041(int numArgs)
{
  double fop1, fop2;

  /* Float >> divFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  if (fop2 == 0.0) {
    FAIL();
  }
  SUCCEED(newFloat(fop1 / fop2));
}


Boolean prim042(int numArgs)
{
  double fop1, fop2;

  /* Float >> addFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 + fop2));
}


Boolean prim043(int numArgs)
{
  double fop1, fop2;

  /* Float >> subFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 - fop2));
}


Boolean prim044(int numArgs)
{
  double fop1, fop2;

  /* float >> mulFloat: */
  fop1 = floatValue(RCVR);
  fop2 = floatValue(ARG(1));
  SUCCEED(newFloat(fop1 * fop2));
}


Boolean prim045(int numArgs)
{
  char numBuffer[20];
  Ulong size;
  ObjPtr object;

  /* Float >> printString */
  sprintf(numBuffer, "%e", floatValue(RCVR));
  size = strlen(numBuffer);
  object = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			  size,
			  FALSE);
  strncpy(getBytes(object), numBuffer, size);
  SUCCEED(object);
}


Boolean prim046(int numArgs)
{
  long iop;

  /* SmallInteger >> asFloat */
  iop = smallIntegerValue(RCVR);
  SUCCEED(newFloat(iop));
}


Boolean prim047(int numArgs)
{
  ObjPtr selector, clazz;
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
  clazz = getClass(getPointer(machine.currentStack,
			      machine.sp - numArgs - 1));
  /* findMethod() sets machine.newClass and machine.newMethod */
  /* save old values of these in case primitive fails later */
  oldClass = machine.newClass;
  oldMethod = machine.newMethod;
  findMethod(clazz, selector);
  if (debugInterpreter) {
    showWhere(clazz, machine.newClass, selector);
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
  return TRUE;
}


Boolean prim048(int numArgs)
{
  /* Object >> become: */
  swapPointers(RCVR, ARG(1));
  SUCCEED(RCVR);
}


Boolean prim049(int numArgs)
{
  Ulong hashval, aux;
  char *cp;
  Ulong size;

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


Boolean prim050(int numArgs)
{
  double fop;

  /* Float >> floor */
  fop = floatValue(RCVR);
  SUCCEED(newSmallInteger((long) floor(fop)));
}


Boolean prim051(int numArgs)
{
  double fop;

  /* Float >> ceil */
  fop = floatValue(RCVR);
  SUCCEED(newSmallInteger((long) ceil(fop)));
}


Boolean prim052(int numArgs)
{
  double fop;

  /* Float >> sin */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(sin(fop)));
}


Boolean prim053(int numArgs)
{
  double fop;

  /* Float >> cos */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(cos(fop)));
}


Boolean prim054(int numArgs)
{
  double fop;

  /* Float >> tan */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(tan(fop)));
}


Boolean prim055(int numArgs)
{
  double fop;

  /* Float >> asin */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(asin(fop)));
}


Boolean prim056(int numArgs)
{
  double fop;

  /* Float >> acos */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(acos(fop)));
}


Boolean prim057(int numArgs)
{
  double fop;

  /* Float >> atan */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(atan(fop)));
}


Boolean prim058(int numArgs)
{
  double fop;

  /* Float >> exp */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(exp(fop)));
}


Boolean prim059(int numArgs)
{
  double fop;

  /* Float >> log */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(log(fop)));
}


Boolean prim060(int numArgs)
{
  double fop;

  /* Float >> sqrt */
  fop = floatValue(RCVR);
  SUCCEED(newFloat(sqrt(fop)));
}


Boolean prim061(int numArgs)
{
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
