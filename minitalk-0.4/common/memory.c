/*
 * memory.c -- object memory
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "utils.h"
#include "machine.h"
#include "struct.h"
#include "memory.h"


/*------------------------------------*/
/* Macros                             */
/*------------------------------------*/


/* two bits of the object pointer are used in coding small integers */

#define IS_SMALLINT		OBJPTR_MSB
#define IS_NEGATIVE		OBJPTR_NSB


/* two flags are coded in the size field of every object */

#define BROKEN_HEART		WORD_MSB
#define HAS_POINTERS		WORD_NSB


/* macros to read and write data in memory */

#define readByte(x)		(*(Byte *)(memory + (x)))
#define writeByte(x, y)		(*(Byte *)(memory + (x)) = (y))
#define readWord(x)		(*(Word *)(memory + (x)))
#define writeWord(x, y)		(*(Word *)(memory + (x)) = (y))
#define readObjPtr(x)		(*(ObjPtr *)(memory + (x)))
#define writeObjPtr(x, y)	(*(ObjPtr *)(memory + (x)) = (y))


/* macros to read and write class and size fields of objects */

#define readClass(o)		readObjPtr(o)
#define writeClass(o, c)	writeObjPtr(o, c)
#define readSize(o)		readWord((o) + sizeof(ObjPtr))
#define writeSize(o, s)		writeWord((o) + sizeof(ObjPtr), s)


/*------------------------------------*/
/* Global Variables                   */
/*------------------------------------*/


Bool debugMemory = false;	/* debug flag, gives statistics when on */

static Byte *memory;		/* object memory where all objects live */


/*------------------------------------*/
/* Garbage Collector                  */
/*------------------------------------*/


/* the semispaces */

static Address toStart;		/* base of "to" semispace in memory */
static Address toEnd;		/* top of "to" semispace in memory */
static Address fromStart;	/* base of "from" semispace in memory */
static Address fromEnd;		/* top of "from" semispace in memory */
static Address toFree;		/* address of first free byte in memory,
				   is always located in "to" semispace */


/* statistical information */

static Word numBytes;		/* number of bytes allocated since last GC,
				   also number of bytes copied during GC */
static Word numObjects;		/* number of objects allocated since last GC,
				   also number of objects copied during GC */


static ObjPtr copyObject(ObjPtr object) {
  Word length;
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
    length *= sizeof(Byte);
  }
  length += sizeof(ObjPtr) + sizeof(Word);
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
    writeByte(toFree, readByte(body));
    toFree++;
    body++;
  }
  return copy;
}


static ObjPtr updatePointer(ObjPtr object) {
  Word size;
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


static void doGC(void) {
  Address tmp;
  Address toScan;
  int i;
  Word size;

  /* print allocation statistics and init collection statistics */
  if (debugMemory) {
    printf("GC: %u bytes in %u objects allocated since last collection\n",
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
    toScan += sizeof(ObjPtr) + sizeof(Word);
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
      toScan += size * sizeof(Byte);
    }
  }
  /* print collection statistics and init allocation statistics */
  if (debugMemory) {
    printf("    %u bytes in %u objects copied during this collection\n",
	   numBytes, numObjects);
    printf("    %lu of %lu bytes are now free\n",
	   (Address) SEMI_SIZE * sizeof(Byte) - numBytes,
	   (Address) SEMI_SIZE * sizeof(Byte));
    numBytes = 0;
    numObjects = 0;
  }
}


static void initGC(void) {
  /* "to" semispace initially starts at 0 */
  toStart = (Address) 0;
  toEnd = toStart + (Address) SEMI_SIZE * sizeof(Byte);
  /* "from" semispace starts where "to" semispace ends */
  fromStart = toEnd;
  fromEnd = fromStart + (Address) SEMI_SIZE * sizeof(Byte);
  /* first free byte depends on how much was loaded */
  toFree = toStart + (Address) machine.memorySize * sizeof(Byte);
  /* init allocation statistics */
  if (debugMemory) {
    numBytes = 0;
    numObjects = 0;
  }
}


static void exitGC(void) {
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


ObjPtr allocateObject(ObjPtr class, Word size, Bool hasPointers) {
  Word length;
  ObjPtr object;
  Word i;

  /* compute length of object in bytes */
  length = size;
  if (hasPointers) {
    length *= sizeof(ObjPtr);
  } else {
    length *= sizeof(Byte);
  }
  length += sizeof(ObjPtr) + sizeof(Word);
  /* check if remaining space is large enough */
  if (toFree + length > toEnd) {
    /* not large enough, do a collection */
    doGC();
    /* ATTENTION: don't forget to update the class pointer! Since */
    /* the class object must also be referenced elsewhere, we are */
    /* sure that no object is actually copied in the process. */
    class = updatePointer(class);
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
  writeClass(object, class);
  if (hasPointers) {
    writeSize(object, size | HAS_POINTERS);
    /* ATTENTION: the following initialization is required! */
    for (i = 0; i < size; i++) {
      /* default object pointer value is nil */
      writeObjPtr(object + sizeof(ObjPtr) + sizeof(Word) +
		  i * sizeof(ObjPtr), machine.nil);
    }
  } else {
    writeSize(object, size);
    /* ATTENTION: the following initialization is optional! */
    for (i = 0; i < size; i++) {
      /* default byte value is 0 */
      writeByte(object + sizeof(ObjPtr) + sizeof(Word) +
		i * sizeof(Byte), 0);
    }
  }
  /* return the object created just now */
  return object;
}


/*------------------------------------*/
/* Object Memory Interface            */
/*------------------------------------*/


ObjPtr getClass(ObjPtr object) {
  if (object & IS_SMALLINT) {
    return getPointer(machine.SmallInteger, VALUE_IN_ASSOCIATION);
  } else {
    return readClass(object);
  }
}


void patchClass(ObjPtr object, ObjPtr class) {
  if (object & IS_SMALLINT) {
    error("patchClass object is small integer");
  }
  writeClass(object, class);
}


Word getSize(ObjPtr object) {
  if (object & IS_SMALLINT) {
    return 0;
  } else {
    return readSize(object) & ~HAS_POINTERS;
  }
}


Bool hasPointers(ObjPtr object) {
  if (object & IS_SMALLINT) {
    return true;
  } else {
    return (readSize(object) & HAS_POINTERS) != 0;
  }
}


Byte *getBytes(ObjPtr object) {
  Word size;

  if (object & IS_SMALLINT) {
    error("getBytes object is small integer");
  }
  size = readSize(object);
  if (size & HAS_POINTERS) {
    error("getBytes object has no bytes");
  }
  return (Byte *) (memory + object + sizeof(ObjPtr) + sizeof(Word));
}


Byte getByte(ObjPtr object, Word index) {
  Word size;

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
  return readByte(object + sizeof(ObjPtr) + sizeof(Word) +
		  index * sizeof(Byte));
}


void setByte(ObjPtr object, Word index, Byte value) {
  Word size;

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
  writeByte(object + sizeof(ObjPtr) + sizeof(Word) +
	    index * sizeof(Byte), value);
}


ObjPtr getPointer(ObjPtr object, Word index) {
  Word size;

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
  return readObjPtr(object + sizeof(ObjPtr) + sizeof(Word) +
		    index * sizeof(ObjPtr));
}


void setPointer(ObjPtr object, Word index, ObjPtr value) {
  Word size;

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
  writeObjPtr(object + sizeof(ObjPtr) + sizeof(Word) +
	      index * sizeof(ObjPtr), value);
}


ObjPtr newSmallInteger(long value) {
  return value | IS_SMALLINT;
}


long smallIntegerValue(ObjPtr smallIntegerObj) {
  return smallIntegerObj & IS_NEGATIVE ?
	 smallIntegerObj : smallIntegerObj & ~IS_SMALLINT;
}


ObjPtr newFloat(double value) {
  ObjPtr floatObj;

  floatObj = allocateObject(getPointer(machine.Float, VALUE_IN_ASSOCIATION),
			    sizeof(double),
			    false);
  * (double *) getBytes(floatObj) = value;
  return floatObj;
}


double floatValue(ObjPtr floatObj) {
  return * (double *) getBytes(floatObj);
}


ObjPtr newCharacter(Byte c) {
  return machine.character[c];
}


Byte characterValue(ObjPtr characterObj) {
  return * (Byte *) getBytes(characterObj);
}


ObjPtr newString(char *string) {
  Word size;
  ObjPtr stringObj;

  size = strlen(string);
  stringObj = allocateObject(getPointer(machine.String, VALUE_IN_ASSOCIATION),
			     size,
			     false);
  memcpy(getBytes(stringObj), string, size);
  return stringObj;
}


char *stringValue(ObjPtr stringObj) {
  return getBytes(stringObj);
}


ObjPtr newSymbol(char *string) {
  ObjPtr symlist;
  Word size1;
  ObjPtr symbol;
  Word size2;
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
			true);
  setPointer(pair,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION));
  setPointer(machine.TheSymbols,
	     VALUE_IN_ASSOCIATION,
	     pair);
  symbol = allocateObject(getPointer(machine.Symbol, VALUE_IN_ASSOCIATION),
			  size1,
			  false);
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


void swapPointers(ObjPtr obj1, ObjPtr obj2) {
  int i;
  Address toScan;
  ObjPtr obj;
  Word size;

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
    toScan += sizeof(ObjPtr) + sizeof(Word);
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
      toScan += size * sizeof(Byte);
    }
  }
}


/*------------------------------------*/
/* Load & Save Image File             */
/*------------------------------------*/


void initMemory(char *imageFileName) {
  FILE *imageFile;

  /* allocate object memory */
  memory = allocate(MEMORY_SIZE * sizeof(Byte));
  /* open image file */
  imageFile = fopen(imageFileName, "rb");
  if (imageFile == NULL) {
    error("cannot open image file '%s' for read", imageFileName);
  }
  /* read machine state */
  if (fread(&machine, sizeof(Machine), 1, imageFile) != 1) {
    error("cannot read machine state from image file");
  }
  /* check image file signature */
  if (machine.signature_1 != SIGNATURE_1 ||
      machine.signature_2 != SIGNATURE_2) {
    error("file '%s' is not an image file", imageFileName);
  }
  /* check image file version number (major only, minor ignored) */
  if (machine.majorVersion != MAJOR_VNUM) {
    error("wrong image file version number");
  }
  /* load object memory */
  if (fread(memory, sizeof(Byte), machine.memorySize, imageFile) !=
      machine.memorySize) {
    error("cannot read objects from image file");
  }
  /* close image file */
  fclose(imageFile);
  /* init garbage collector */
  initGC();
}


void exitMemory(char *imageFileName) {
  FILE *imageFile;

  /* exit garbage collector */
  exitGC();
  /* open image file */
  imageFile = fopen(imageFileName, "wb");
  if (imageFile == NULL) {
    error("cannot open image file '%s' for write", imageFileName);
  }
  /* write machine state */
  if (fwrite(&machine, sizeof(Machine), 1, imageFile) != 1) {
    error("cannot write machine state to image file");
  }
  /* save object memory */
  if (fwrite(memory, sizeof(Byte), machine.memorySize, imageFile) !=
      machine.memorySize) {
    error("cannot write objects to image file");
  }
  /* close image file */
  fclose(imageFile);
  /* free object memory */
  release(memory);
}
