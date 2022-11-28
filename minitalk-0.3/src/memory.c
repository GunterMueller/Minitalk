
/*
 * MEMORY.C	object memory
 */


#include "minitalk.h"


/* memory usage information is printed if this flag is set */

Boolean debugMemory = FALSE;


/* this is the virtual machine which executes the bytecodes */

Machine machine;


/* this is the memory where all the objects live */

Uchar *memory;


/* macros to read and write data in memory */

#define readUchar(x)		(*(Uchar *)(memory + (x)))
#define writeUchar(x, y)	(*(Uchar *)(memory + (x)) = (y))
#define readUlong(x)		(*(Ulong *)(memory + (x)))
#define writeUlong(x, y)	(*(Ulong *)(memory + (x)) = (y))
#define readObjPtr(x)		(*(ObjPtr *)(memory + (x)))
#define writeObjPtr(x, y)	(*(ObjPtr *)(memory + (x)) = (y))


/* macros to read and write class and size fields of objects */

#define readClass(o)		readObjPtr(o)
#define writeClass(o, c)	writeObjPtr(o, c)
#define readSize(o)		readUlong((o) + sizeof(ObjPtr))
#define writeSize(o, s)		writeUlong((o) + sizeof(ObjPtr), s)


/*------------------------------------*/
/* Garbage Collector                  */
/*------------------------------------*/


/* the semispaces */

Address toStart;	/* base of "to" semispace in object memory */
Address toEnd;		/* top of "to" semispace in object memory */
Address fromStart;	/* base of "from" semispace in object memory */
Address fromEnd;	/* top of "from" semispace in object memory */
Address toFree;		/* address of first free byte in object memory,
			   which is always located in "to" semispace */


/* statistical information */

Ulong numBytes;		/* number of bytes allocated since last GC */
			/* also used for number of bytes copied during GC */
Ulong numObjects;	/* number of objects allocated since last GC */
			/* also used for number of objects copied during GC */


ObjPtr copyObject(ObjPtr object)
{
  Ulong length;
  ObjPtr copy;
  Address body;

  /* REMARK: small integers are provably never copied */
  /* read size of object */
  length = readSize(object);
  /* compute length of object dependent on pointer flag */
  if (length & HAS_POINTERS) {
    length &= ~HAS_POINTERS;
    length *= sizeof(ObjPtr);
  } else {
    length *= sizeof(Uchar);
  }
  length += sizeof(ObjPtr) + sizeof(Ulong);
  /* if not enough space, something goes terribly wrong */
  if (toFree + length > toEnd) {
    error("copyObject has no space");
  }
  /* update collection statistics */
  if (debugMemory) {
    numBytes += length;
    numObjects++;
  }
  /* copy the object to free memory and return new address */
  copy = (ObjPtr) toFree;
  body = (Address) object;
  while (length--) {
    writeUchar(toFree++, readUchar(body++));
  }
  return copy;
}


ObjPtr updatePointer(ObjPtr object)
{
  Ulong size;
  ObjPtr copy;

  /* a relocated small integer is the small integer itself */
  if (object & IS_SMALLINT) {
    return object;
  }
  /* read size and check the broken-heart flag */
  size = readSize(object);
  if (size & BROKEN_HEART) {
    /* object has already been copied, forward pointer is in class slot */
    return readClass(object);
  } else {
    /* object has not been copied yet, so do this now */
    copy = copyObject(object);
    /* in the original object: set broken-heart flag and forward pointer */
    writeSize(object, size | BROKEN_HEART);
    writeClass(object, copy);
    /* return pointer to copied object */
    return copy;
  }
}


#define UPDATE(reg)	reg = updatePointer(reg)


void doGC(void)
{
  Address tmp;
  Address toScan;
  int i;
  Ulong size;

  /* print allocation statistics and init collection statistics */
  if (debugMemory) {
    printf("GC: %lu bytes in %lu objects allocated since last collection\n",
	   numBytes, numObjects);
    numBytes = 0;
    numObjects = 0;
  }
  /* flip semispaces */
  tmp = toStart;
  toStart = fromStart;
  fromStart = tmp;
  tmp = toEnd;
  toEnd = fromEnd;
  fromEnd = tmp;
  /* set-up free and scan pointers */
  toFree = toStart;
  toScan = toFree;
  /* first relocate the roots of the world, i.e. all object registers */
  UPDATE(machine.nil);
  UPDATE(machine.false);
  UPDATE(machine.true);
  for (i = 0; i < 256; i++) {
    UPDATE(machine.character[i]);
  }
  UPDATE(machine.UndefinedObject);
  UPDATE(machine.False);
  UPDATE(machine.True);
  UPDATE(machine.Character);
  UPDATE(machine.SmallInteger);
  UPDATE(machine.Float);
  UPDATE(machine.LinkedObject);
  UPDATE(machine.Association);
  UPDATE(machine.Dictionary);
  UPDATE(machine.Array);
  UPDATE(machine.ByteArray);
  UPDATE(machine.String);
  UPDATE(machine.Symbol);
  UPDATE(machine.CompiledMethod);
  UPDATE(machine.BlockContext);
  UPDATE(machine.MethodContext);
  UPDATE(machine.Class);
  UPDATE(machine.Metaclass);
  UPDATE(machine.TheSymbols);
  UPDATE(machine.MiniTalk);
  UPDATE(machine.currentActiveContext);
  UPDATE(machine.currentHomeContext);
  UPDATE(machine.currentSender);
  UPDATE(machine.currentCaller);
  UPDATE(machine.currentClass);
  UPDATE(machine.currentMethod);
  UPDATE(machine.currentSelector);
  UPDATE(machine.currentBytecodes);
  UPDATE(machine.currentLiterals);
  UPDATE(machine.currentReceiver);
  UPDATE(machine.currentTemporaries);
  UPDATE(machine.currentStack);
  UPDATE(machine.newClass);
  UPDATE(machine.newMethod);
  UPDATE(machine.newContext);
  UPDATE(machine.compilerClass);
  UPDATE(machine.compilerCode);
  UPDATE(machine.compilerLiteral);
  UPDATE(machine.compilerLiterals);
  UPDATE(machine.compilerMethod);
  UPDATE(machine.compilerAssociation);
  /* then relocate the rest of the world iteratively */
  while (toScan != toFree) {
    /* there is another object to scan */
    /* update class pointer of object */
    writeClass((ObjPtr) toScan, updatePointer(readClass((ObjPtr) toScan)));
    /* read size and let scan pointer point to object's body */
    size = readSize((ObjPtr) toScan);
    toScan += sizeof(ObjPtr) + sizeof(Ulong);
    /* inspect pointer flag */
    if (size & HAS_POINTERS) {
      /* object has pointers, update them */
      size &= ~HAS_POINTERS;
      while (size--) {
	writeObjPtr(toScan, updatePointer(readObjPtr(toScan)));
	toScan += sizeof(ObjPtr);
      }
    } else {
      /* object has no pointers, skip it */
      toScan += size * sizeof(Uchar);
    }
  }
  /* print collection statistics and init allocation statistics */
  if (debugMemory) {
    printf("    %lu bytes in %lu objects copied during this collection\n",
	   numBytes, numObjects);
    printf("    %lu of %lu bytes are now free\n",
	   (Address) SEMISIZE * KILO * sizeof(Uchar) - numBytes,
	   (Address) SEMISIZE * KILO * sizeof(Uchar));
    numBytes = 0;
    numObjects = 0;
  }
}


void initGC(void)
{
  /* "to" semispace initially starts at 0 */
  toStart = (Address) 0;
  toEnd = toStart + (Address) SEMISIZE * KILO * sizeof(Uchar);
  /* "from" semispace starts where "to" semispace ends */
  fromStart = toEnd;
  fromEnd = fromStart + (Address) SEMISIZE * KILO * sizeof(Uchar);
  /* first free byte depends on how much was loaded */
  toFree = toStart + (Address) machine.memorySize * sizeof(Uchar);
  /* init allocation statistics */
  if (debugMemory) {
    numBytes = 0;
    numObjects = 0;
  }
}


void exitGC(void)
{
  /* do a collection to get objects compacted */
  doGC();
  /* check whether in lower semispace */
  if (toStart != (Address) 0) {
    /* it's the upper one, so collect again to switch semispaces */
    doGC();
  }
  /* compute total size of all objects in bytes */
  machine.memorySize = toFree - toStart;
}


/*------------------------------------*/
/* Object Allocator                   */
/*------------------------------------*/


ObjPtr allocateObject(ObjPtr clazz, Ulong size, Boolean hasPointers)
{
  Ulong length;
  ObjPtr object;
  Ulong i;

  /* compute length of object in bytes */
  length = size;
  if (hasPointers) {
    length *= sizeof(ObjPtr);
  } else {
    length *= sizeof(Uchar);
  }
  length += sizeof(ObjPtr) + sizeof(Ulong);
  /* check if remaining space is large enough */
  if (toFree + length > toEnd) {
    /* not large enough, do a collection */
    doGC();
    /* ATTENTION: don't forget to update the class pointer! Since */
    /* the class object must also be referenced elsewhere, we are */
    /* sure that no object is actually copied in the process. */
    clazz = updatePointer(clazz);
    /* check for object memory overflow */
    if (toFree + length > toEnd) {
      error("object memory exhausted");
    }
  }
  /* allocate the requested space */
  object = (ObjPtr) toFree;
  toFree += length;
  /* update allocation statistics */
  if (debugMemory) {
    numBytes += length;
    numObjects++;
  }
  /* set class and size; init fields */
  writeClass(object, clazz);
  if (hasPointers) {
    writeSize(object, size | HAS_POINTERS);
    /* ATTENTION: the following initialization is required! */
    for (i = 0; i < size; i++) {
      /* default object pointer value is nil */
      writeObjPtr(object + sizeof(ObjPtr) + sizeof(Ulong) +
		  i * sizeof(ObjPtr), machine.nil);
    }
  } else {
    writeSize(object, size);
    /* ATTENTION: the following initialization is optional! */
    for (i = 0; i < size; i++) {
      /* default byte value is 0 */
      writeUchar(object + sizeof(ObjPtr) + sizeof(Ulong) +
		 i * sizeof(Uchar), 0);
    }
  }
  /* return the object created just now */
  return object;
}


/*------------------------------------*/
/* Object Memory Interface            */
/*------------------------------------*/


ObjPtr getClass(ObjPtr object)
{
  if (object & IS_SMALLINT) {
    return getPointer(machine.SmallInteger, VALUE_IN_ASSOCIATION);
  } else {
    return readClass(object);
  }
}


Ulong getSize(ObjPtr object)
{
  if (object & IS_SMALLINT) {
    return 0;
  } else {
    return readSize(object) & ~HAS_POINTERS;
  }
}


Boolean hasPointers(ObjPtr object)
{
  if (object & IS_SMALLINT) {
    return TRUE;
  } else {
    return (readSize(object) & HAS_POINTERS) != 0;
  }
}


Uchar *getBytes(ObjPtr object)
{
  Ulong size;

  if (object & IS_SMALLINT) {
    error("getBytes object is small integer");
  }
  size = readSize(object);
  if (size & HAS_POINTERS) {
    error("getBytes object has no bytes");
  }
  return (Uchar *) (memory + object + sizeof(ObjPtr) + sizeof(Ulong));
}


Uchar getByte(ObjPtr object, Ulong index)
{
  Ulong size;

  if (object & IS_SMALLINT) {
    error("getByte object is small integer");
  }
  size = readSize(object);
  if (size & HAS_POINTERS) {
    error("getByte object has no bytes");
  }
  if (index >= size) {
    error("getByte index out of range");
  }
  return readUchar(object + sizeof(ObjPtr) + sizeof(Ulong) +
		   index * sizeof(Uchar));
}


void setByte(ObjPtr object, Ulong index, Uchar value)
{
  Ulong size;

  if (object & IS_SMALLINT) {
    error("setByte object is small integer");
  }
  size = readSize(object);
  if (size & HAS_POINTERS) {
    error("setByte object has no bytes");
  }
  if (index >= size) {
    error("setByte index out of range");
  }
  writeUchar(object + sizeof(ObjPtr) + sizeof(Ulong) +
	     index * sizeof(Uchar), value);
}


ObjPtr getPointer(ObjPtr object, Ulong index)
{
  Ulong size;

  if (object & IS_SMALLINT) {
    error("getPointer object is small integer");
  }
  size = readSize(object);
  if (!(size & HAS_POINTERS)) {
    error("getPointer object has no pointers");
  }
  size &= ~HAS_POINTERS;
  if (index >= size) {
    error("getPointer index out of range");
  }
  return readObjPtr(object + sizeof(ObjPtr) + sizeof(Ulong) +
		    index * sizeof(ObjPtr));
}


void setPointer(ObjPtr object, Ulong index, ObjPtr value)
{
  Ulong size;

  if (object & IS_SMALLINT) {
    error("setPointer object is small integer");
  }
  size = readSize(object);
  if (!(size & HAS_POINTERS)) {
    error("setPointer object has no pointers");
  }
  size &= ~HAS_POINTERS;
  if (index >= size) {
    error("setPointer index out of range");
  }
  writeObjPtr(object + sizeof(ObjPtr) + sizeof(Ulong) +
	      index * sizeof(ObjPtr), value);
}


ObjPtr newSmallInteger(long value)
{
  return value | IS_SMALLINT;
}


long smallIntegerValue(ObjPtr smallIntegerObj)
{
  return smallIntegerObj & IS_NEGATIVE ?
	 smallIntegerObj : smallIntegerObj & ~IS_SMALLINT;
}


ObjPtr newFloat(double value)
{
  ObjPtr floatObj;

  floatObj = allocateObject(getPointer(machine.Float, VALUE_IN_ASSOCIATION),
			    sizeof(double),
			    FALSE);
  * (double *) getBytes(floatObj) = value;
  return floatObj;
}


double floatValue(ObjPtr floatObj)
{
  return * (double *) getBytes(floatObj);
}


ObjPtr newCharacter(Uchar c)
{
  return machine.character[c];
}


Uchar characterValue(ObjPtr characterObj)
{
  return * (Uchar *) getBytes(characterObj);
}


ObjPtr newString(char *string)
{
  Ulong size;
  ObjPtr newString;

  size = strlen(string);
  newString = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			     size,
			     FALSE);
  memcpy(getBytes(newString), string, size);
  return newString;
}


char *stringValue(ObjPtr stringObj)
{
  return getBytes(stringObj);
}


ObjPtr newSymbol(char *string)
{
  ObjPtr symlist;
  Ulong size1;
  ObjPtr symbol;
  Ulong size2;
  ObjPtr pair;

  /* search symbol list */
  symlist = getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION);
  size1 = strlen(string);
  while (symlist != machine.nil) {
    symbol = getPointer(symlist, OBJECT_IN_LINKEDOBJECT);
    size2 = getSize(symbol);
    if (size1 == size2 &&
	strncmp(string, getBytes(symbol), size1) == 0) {
      /* symbol found, return it */
      return symbol;
    }
    symlist = getPointer(symlist, NEXTLINK_IN_LINKEDOBJECT);
  }
  /* symbol not found, create new one and link it to the symbol table */
  /* ATTENTION: the order of the following events is crucial! */
  pair = allocateObject(getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION),
			SIZE_OF_LINKEDOBJECT,
			TRUE);
  setPointer(pair,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION));
  setPointer(machine.TheSymbols,
	     VALUE_IN_ASSOCIATION,
	     pair);
  symbol = allocateObject(getPointer(machine.Symbol, VALUE_IN_ASSOCIATION),
			  size1,
			  FALSE);
  memcpy(getBytes(symbol), string, size1);
  /* ATTENTION: do not use the value of 'pair' any longer! */
  setPointer(getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION),
	     OBJECT_IN_LINKEDOBJECT,
	     symbol);
  /* return the symbol created just now */
  return symbol;
}


/*------------------------------------*/
/* Swap Pointers Primitive            */
/*------------------------------------*/


#define SWAPP(reg)	if (reg == obj1) { reg = obj2; } else \
			if (reg == obj2) { reg = obj1; }


void swapPointers(ObjPtr obj1, ObjPtr obj2)
{
  int i;
  Address toScan;
  ObjPtr obj;
  Ulong size;

  /* first transform the roots of the world, i.e. all object registers */
  SWAPP(machine.nil);
  SWAPP(machine.false);
  SWAPP(machine.true);
  for (i = 0; i < 256; i++) {
    SWAPP(machine.character[i]);
  }
  SWAPP(machine.UndefinedObject);
  SWAPP(machine.False);
  SWAPP(machine.True);
  SWAPP(machine.Character);
  SWAPP(machine.SmallInteger);
  SWAPP(machine.Float);
  SWAPP(machine.LinkedObject);
  SWAPP(machine.Association);
  SWAPP(machine.Dictionary);
  SWAPP(machine.Array);
  SWAPP(machine.ByteArray);
  SWAPP(machine.String);
  SWAPP(machine.Symbol);
  SWAPP(machine.CompiledMethod);
  SWAPP(machine.BlockContext);
  SWAPP(machine.MethodContext);
  SWAPP(machine.Class);
  SWAPP(machine.Metaclass);
  SWAPP(machine.TheSymbols);
  SWAPP(machine.MiniTalk);
  SWAPP(machine.currentActiveContext);
  SWAPP(machine.currentHomeContext);
  SWAPP(machine.currentSender);
  SWAPP(machine.currentCaller);
  SWAPP(machine.currentClass);
  SWAPP(machine.currentMethod);
  SWAPP(machine.currentSelector);
  SWAPP(machine.currentBytecodes);
  SWAPP(machine.currentLiterals);
  SWAPP(machine.currentReceiver);
  SWAPP(machine.currentTemporaries);
  SWAPP(machine.currentStack);
  SWAPP(machine.newClass);
  SWAPP(machine.newMethod);
  SWAPP(machine.newContext);
  SWAPP(machine.compilerClass);
  SWAPP(machine.compilerCode);
  SWAPP(machine.compilerLiteral);
  SWAPP(machine.compilerLiterals);
  SWAPP(machine.compilerMethod);
  SWAPP(machine.compilerAssociation);
  /* then transform the rest of the world iteratively */
  toScan = toStart;
  while (toScan != toFree) {
    /* there is another object to scan */
    /* transform class pointer of object */
    obj = readClass((ObjPtr) toScan);
    if (obj == obj1) {
      writeClass((ObjPtr) toScan, obj2);
    } else
    if (obj == obj2) {
      writeClass((ObjPtr) toScan, obj1);
    }
    /* read size and let scan pointer point to object's body */
    size = readSize((ObjPtr) toScan);
    toScan += sizeof(ObjPtr) + sizeof(Ulong);
    /* inspect pointer flag */
    if (size & HAS_POINTERS) {
      /* object has pointers, transform them */
      size &= ~HAS_POINTERS;
      while (size--) {
	obj = readObjPtr(toScan);
	if (obj == obj1) {
	  writeObjPtr(toScan, obj2);
	} else
	if (obj == obj2) {
	  writeObjPtr(toScan, obj1);
	}
	toScan += sizeof(ObjPtr);
      }
    } else {
      /* object has no pointers, skip it */
      toScan += size * sizeof(Uchar);
    }
  }
}


/*------------------------------------*/
/* Load & Save Image File             */
/*------------------------------------*/


void initMemory(char *fileName)
{
  FILE *imageFile;

  /* allocate object memory */
  memory = (Uchar *) malloc(MEMSIZE * KILO * sizeof(Uchar));
  if (memory == NULL) {
    error("cannot allocate object memory");
  }
  /* open image file */
  imageFile = fopen(fileName, "rb");
  if (imageFile == NULL) {
    error("cannot open image file for read");
  }
  /* read machine state */
  if (fread(&machine, sizeof(Machine), 1, imageFile) != 1) {
    error("cannot read machine state from image file");
  }
  /* check image file version number */
  if (machine.majorVersion != MAJOR_VNUM ||
      machine.minorVersion != MINOR_VNUM) {
    error("wrong image file version number");
  }
  /* load object memory */
  if (fread(memory, sizeof(Uchar), machine.memorySize, imageFile) !=
      machine.memorySize) {
    error("cannot read objects from image file");
  }
  /* close image file */
  fclose(imageFile);
  /* init garbage collector */
  initGC();
}


void exitMemory(char *fileName)
{
  FILE *imageFile;

  /* exit garbage collector */
  exitGC();
  /* open image file */
  imageFile = fopen(fileName, "wb");
  if (imageFile == NULL) {
    error("cannot open image file for write");
  }
  /* write machine state */
  if (fwrite(&machine, sizeof(Machine), 1, imageFile) != 1) {
    error("cannot write machine state to image file");
  }
  /* save object memory */
  if (fwrite(memory, sizeof(Uchar), machine.memorySize, imageFile) !=
      machine.memorySize) {
    error("cannot write objects to image file");
  }
  /* close image file */
  fclose(imageFile);
  /* free object memory */
  free(memory);
}
