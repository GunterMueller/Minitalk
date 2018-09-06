/*
 * mkimage.c -- image file generator
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "utils.h"
#include "machine.h"
#include "struct.h"
#include "memory.h"
#include "compiler.h"
#include "tree.h"
#include "scanner.h"
#include "parser.h"


/*------------------------------------*/
/* Object Generation And Access       */
/*------------------------------------*/


static ObjPtr newArray(Word size) {
  ObjPtr array;

  array =
    allocateObject(getPointer(machine.Array, VALUE_IN_ASSOCIATION),
		   size,
		   true);
  return array;
}


static ObjPtr newByteArray(Byte *bytes, Word size) {
  ObjPtr byteArray;

  byteArray =
    allocateObject(getPointer(machine.ByteArray, VALUE_IN_ASSOCIATION),
		   size,
		   false);
  memcpy(getBytes(byteArray), bytes, size);
  return byteArray;
}


static ObjPtr newCompiledMethod(ObjPtr selector,
                                ObjPtr primitive,
                                ObjPtr numberargs,
                                ObjPtr tempsize,
                                ObjPtr stacksize,
                                ObjPtr bytecodes,
                                ObjPtr literals) {
  ObjPtr compiledMethod;

  compiledMethod =
    allocateObject(getPointer(machine.CompiledMethod, VALUE_IN_ASSOCIATION),
		   SIZE_OF_COMPILEDMETHOD,
		   true);
  setPointer(compiledMethod, SELECTOR_IN_COMPILEDMETHOD, selector);
  setPointer(compiledMethod, PRIMITIVE_IN_COMPILEDMETHOD, primitive);
  setPointer(compiledMethod, NUMBERARGS_IN_COMPILEDMETHOD, numberargs);
  setPointer(compiledMethod, TEMPSIZE_IN_COMPILEDMETHOD, tempsize);
  setPointer(compiledMethod, STACKSIZE_IN_COMPILEDMETHOD, stacksize);
  setPointer(compiledMethod, BYTECODES_IN_COMPILEDMETHOD, bytecodes);
  setPointer(compiledMethod, LITERALS_IN_COMPILEDMETHOD, literals);
  return compiledMethod;
}


static ObjPtr newMethodContext(ObjPtr sender,
                               ObjPtr instptr,
                               ObjPtr stackptr,
                               ObjPtr stack,
                               ObjPtr class,
                               ObjPtr method,
                               ObjPtr receiver,
                               ObjPtr temporaries) {
  ObjPtr context;

  context =
    allocateObject(getPointer(machine.MethodContext, VALUE_IN_ASSOCIATION),
		   SIZE_OF_METHODCONTEXT,
		   true);
  setPointer(context, SENDER_IN_METHODCONTEXT, sender);
  setPointer(context, INSTPTR_IN_METHODCONTEXT, instptr);
  setPointer(context, STACKPTR_IN_METHODCONTEXT, stackptr);
  setPointer(context, STACK_IN_METHODCONTEXT, stack);
  setPointer(context, CLASS_IN_METHODCONTEXT, class);
  setPointer(context, METHOD_IN_METHODCONTEXT, method);
  setPointer(context, RECEIVER_IN_METHODCONTEXT, receiver);
  setPointer(context, TEMPORARIES_IN_METHODCONTEXT, temporaries);
  return context;
}


static ObjPtr newDictionary(void) {
  ObjPtr dictionary;

  dictionary =
    allocateObject(getPointer(machine.Dictionary, VALUE_IN_ASSOCIATION),
		   SIZE_OF_DICTIONARY,
		   true);
  return dictionary;
}


/*------------------------------------*/
/* Standard Classes                   */
/*------------------------------------*/


static void forEachClassDo(char *classListName,
                           void (*function)(char *fileName)) {
  FILE *classListFile;
  char path[LINE_SIZE];
  char line[LINE_SIZE];
  char file[LINE_SIZE];
  char *cp, *className;

  /* open file with list of standard classes */
  classListFile = fopen(classListName, "rt");
  if (classListFile == NULL) {
    error("cannot open class list file '%s'", classListName);
  }
  /* extract path from class list file name */
  strcpy(path, classListName);
  cp = path + strlen(path);
  while (1) {
    if (*cp == '/') {
      cp++;
      break;
    }
    if (cp == path) {
      break;
    }
    cp--;
  }
  *cp = '\0';
  /* work on class list file line by line */
  while (fgets(line, LINE_SIZE, classListFile) != NULL) {
    /* separate class name */
    cp = line;
    while (*cp == ' ' || *cp == '\t') {
      cp++;
    }
    if (*cp == '#' || *cp == '\n') {
      continue;
    }
    className = cp;
    while (*cp != ' ' && *cp != '\t' && *cp != '#' && *cp != '\n') {
      cp++;
    }
    *cp = '\0';
    /* convert classname to filename */
    strcpy(file, path);
    strcat(file, className);
    strcat(file, ".mt");
    /* apply function to filename */
    (*function)(file);
  }
  /* close file with list of standard classes */
  fclose(classListFile);
}


/*------------------------------------*/
/* Image File Creation                */
/*------------------------------------*/


static void createImageFile(char *fileName) {
  FILE *imageFile;

  /* try to create image file */
  imageFile = fopen(fileName, "wb");
  if (imageFile == NULL) {
    error("cannot create image file");
  }
  /* write initial machine state */
  machine.signature_1 = SIGNATURE_1;
  machine.signature_2 = SIGNATURE_2;
  machine.majorVersion = MAJOR_VNUM;
  machine.minorVersion = MINOR_VNUM;
  machine.memoryStart = sizeof(Machine);
  machine.memorySize = 0;
  if (fwrite(&machine, sizeof(Machine), 1, imageFile) != 1) {
    error("cannot write initial machine state");
  }
  /* close image file */
  fclose(imageFile);
}


/*------------------------------------*/
/* How A Programmer Creates The World */
/*------------------------------------*/


#define THE_SYMBOLS		"TheSymbols"


static void bigBangPart1(void) {
  int i;
  ObjPtr symbol;
  ObjPtr symbols;

  /* create nil: this will allow to work alloacteObject() correctly */
  machine.nil = allocateObject((ObjPtr) 0, 0, false);
  /* now false and true can be created */
  machine.false = allocateObject(machine.nil, 0, false);
  machine.true = allocateObject(machine.nil, 0, false);
  /* then, the characters */
  for (i = 0; i < 256; i++) {
    machine.character[i] = allocateObject(machine.nil, 1, false);
    setByte(machine.character[i], 0, i);
  }
  /* finally, the symbols */
  symbol = allocateObject(machine.nil, strlen(THE_SYMBOLS), false);
  strncpy(getBytes(symbol), THE_SYMBOLS, strlen(THE_SYMBOLS));
  symbols = allocateObject(machine.nil, SIZE_OF_LINKEDOBJECT, true);
  setPointer(symbols, OBJECT_IN_LINKEDOBJECT, symbol);
  machine.TheSymbols = allocateObject(machine.nil, SIZE_OF_ASSOCIATION, true);
  setPointer(machine.TheSymbols, KEY_IN_ASSOCIATION, symbol);
  setPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION, symbols);
}


static void bigBangPart2(void) {
  int i;
  ObjPtr symbols;
  ObjPtr symbol;

  /* patch classes of objects created in part 1 */
  patchClass(machine.nil,
             getPointer(machine.UndefinedObject, VALUE_IN_ASSOCIATION));
  patchClass(machine.false,
             getPointer(machine.False, VALUE_IN_ASSOCIATION));
  patchClass(machine.true,
             getPointer(machine.True, VALUE_IN_ASSOCIATION));
  for (i = 0; i < 256; i++) {
    patchClass(machine.character[i],
               getPointer(machine.Character, VALUE_IN_ASSOCIATION));
  }
  patchClass(machine.TheSymbols,
             getPointer(machine.Association, VALUE_IN_ASSOCIATION));
  symbols = getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION);
  while (symbols != machine.nil) {
    patchClass(symbols,
               getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION));
    symbol = getPointer(symbols, OBJECT_IN_LINKEDOBJECT);
    patchClass(symbol,
               getPointer(machine.Symbol, VALUE_IN_ASSOCIATION));
    symbols = getPointer(symbols, NEXTLINK_IN_LINKEDOBJECT);
  }
}


/*------------------------------------*/
/* Chunk Handling                     */
/*------------------------------------*/


static char *nextChunk(FILE *sourceFile) {
  static char chunkBuffer[MAX_CHUNKSIZE];
  char *cp;
  int c;

  cp = chunkBuffer;
  while (1) {
    c = getc(sourceFile);
    if (c == EOF) {
      break;
    }
    if (c == '!') {
      c = getc(sourceFile);
      if (c != '!') {
	break;
      }
    }
    *cp++ = c;
    if (cp == &chunkBuffer[MAX_CHUNKSIZE]) {
      error("source file chunk too big");
    }
  }
  *cp = '\0';
  while (c == ' ' || c == '\t' || c == '\n') {
    c = getc(sourceFile);
  }
  if (c != EOF) {
    ungetc(c, sourceFile);
  }
  return chunkBuffer;
}


/*------------------------------------*/
/* Analyze Class Creation Expression  */
/*------------------------------------*/


static char **analyzeClassCreationExpression(char *aString) {
  static char textBuffer[MAX_CHUNKSIZE];
  static char *tokens[7];
  char *cp;

  initScanner(aString);
  cp = textBuffer;
  nextToken();
  /* superclass name */
  if (tokenType != T_IDENT) {
    return NULL;
  }
  tokens[0] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'subclass:', 'variableSubclass:', or something like that */
  if (tokenType != T_KEYWORD) {
    return NULL;
  }
  tokens[1] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* T_HASH */
  if (tokenType != T_HASH) {
    return NULL;
  }
  nextToken();
  /* class name */
  if (tokenType != T_IDENT) {
    return NULL;
  }
  tokens[2] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'instanceVariableNames:' */
  if (tokenType != T_KEYWORD ||
      strcmp(stringBuffer, "instanceVariableNames:") != 0) {
    return NULL;
  }
  nextToken();
  /* a string */
  if (tokenType != T_STRING) {
    return NULL;
  }
  tokens[3] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'classVariableNames:' */
  if (tokenType != T_KEYWORD ||
      strcmp(stringBuffer, "classVariableNames:") != 0) {
    return NULL;
  }
  nextToken();
  /* a string */
  if (tokenType != T_STRING) {
    return NULL;
  }
  tokens[4] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'poolDictionaries:' */
  if (tokenType != T_KEYWORD ||
      strcmp(stringBuffer, "poolDictionaries:") != 0) {
    return NULL;
  }
  nextToken();
  /* a string */
  if (tokenType != T_STRING) {
    return NULL;
  }
  tokens[5] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'category:' */
  if (tokenType != T_KEYWORD ||
      strcmp(stringBuffer, "category:") != 0) {
    return NULL;
  }
  nextToken();
  /* a string */
  if (tokenType != T_STRING) {
    return NULL;
  }
  tokens[6] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* T_END */
  if (tokenType != T_END) {
    return NULL;
  }
  /* return token array */
  return tokens;
}


/*------------------------------------*/
/* Analyze Category Reader Expression */
/*------------------------------------*/


static char **analyzeCategoryReaderExpression(char *aString) {
  static char textBuffer[MAX_CHUNKSIZE];
  static char *tokens[3];
  char *cp;

  initScanner(aString);
  cp = textBuffer;
  nextToken();
  /* class name */
  if (tokenType != T_IDENT) {
    return NULL;
  }
  tokens[0] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* 'class' or not 'class' this is here the question... */
  if (tokenType == T_IDENT &&
      strcmp(stringBuffer, "class") == 0) {
    tokens[1] = cp;
    strcpy(cp, stringBuffer);
    cp += strlen(stringBuffer) + 1;
    nextToken();
  } else {
    tokens[1] = NULL;
  }
  /* 'methodsFor:' */
  if (tokenType != T_KEYWORD ||
      strcmp(stringBuffer, "methodsFor:") != 0) {
    return NULL;
  }
  nextToken();
  /* a string */
  if (tokenType != T_STRING) {
    return NULL;
  }
  tokens[2] = cp;
  strcpy(cp, stringBuffer);
  cp += strlen(stringBuffer) + 1;
  nextToken();
  /* T_END */
  if (tokenType != T_END) {
    return NULL;
  }
  /* return token array */
  return tokens;
}


/*------------------------------------*/
/* Classes Needed By Interpreter      */
/*------------------------------------*/


struct {
  char *name;
  ObjPtr *variable;
  Bool initialized;
} classesNeededByInterpreter[] = {
  { "UndefinedObject", &machine.UndefinedObject, false },
  { "False",           &machine.False,           false },
  { "True",            &machine.True,            false },
  { "Character",       &machine.Character,       false },
  { "SmallInteger",    &machine.SmallInteger,    false },
  { "Float",           &machine.Float,           false },
  { "LinkedObject",    &machine.LinkedObject,    false },
  { "Association",     &machine.Association,     false },
  { "Dictionary",      &machine.Dictionary,      false },
  { "Array",           &machine.Array,           false },
  { "ByteArray",       &machine.ByteArray,       false },
  { "String",          &machine.String,          false },
  { "Symbol",          &machine.Symbol,          false },
  { "CompiledMethod",  &machine.CompiledMethod,  false },
  { "BlockContext",    &machine.BlockContext,    false },
  { "MethodContext",   &machine.MethodContext,   false },
  { "Class",           &machine.Class,           false },
  { "Metaclass",       &machine.Metaclass,       false }
};


static ObjPtr *classNeededByInterpreter(char *className) {
  int i;

  for (i = 0; i < sizeof(classesNeededByInterpreter) /
		  sizeof(classesNeededByInterpreter[0]); i++) {
    if (strcmp(classesNeededByInterpreter[i].name, className) == 0) {
      classesNeededByInterpreter[i].initialized = true;
      return classesNeededByInterpreter[i].variable;
    }
  }
  return NULL;
}


static void checkClassesNeededByInterpreter(void) {
  int i;
  char errorString[100];

  for (i = 0; i < sizeof(classesNeededByInterpreter) /
		  sizeof(classesNeededByInterpreter[0]); i++) {
    if (!classesNeededByInterpreter[i].initialized) {
      sprintf(errorString,
	      "class %s needed but not created",
	      classesNeededByInterpreter[i].name);
      error(errorString);
    }
  }
}


/*------------------------------------*/
/* Create Classes From Source File    */
/*------------------------------------*/


#define MAX_NUMBER_CLASSES		100


static int numberClasses = 0;


struct classStruct {
  char *className;
  ObjPtr classAssociation;
} classArray[MAX_NUMBER_CLASSES];


static void freeClassArray(void) {
  int i;

  for (i = 0; i < numberClasses; i++) {
    free(classArray[i].className);
  }
}


static ObjPtr lookupClass(char *className) {
  int i;
  char errorString[100];

  for (i = 0; i < numberClasses; i++) {
    if (strcmp(classArray[i].className, className) == 0) {
      return classArray[i].classAssociation;
    }
  }
  strcpy(errorString, "cannot find class ");
  strcat(errorString, className);
  error(errorString);
  return machine.nil;
}


static ObjPtr makeClassAssociation(char *className) {
  ObjPtr metaclass;
  ObjPtr class;
  ObjPtr association;
  ObjPtr key;
  ObjPtr symbols;

  metaclass = allocateObject(machine.nil, SIZE_OF_METACLASS, true);
  class = allocateObject(metaclass, SIZE_OF_CLASS, true);
  association = allocateObject(machine.nil, SIZE_OF_ASSOCIATION, true);
  setPointer(association, VALUE_IN_ASSOCIATION, class);
  key = allocateObject(machine.nil, strlen(className), false);
  strncpy(getBytes(key), className, strlen(className));
  setPointer(association, KEY_IN_ASSOCIATION, key);
  symbols = allocateObject(machine.nil, SIZE_OF_LINKEDOBJECT, true);
  setPointer(symbols, OBJECT_IN_LINKEDOBJECT, key);
  setPointer(symbols,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION));
  setPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION, symbols);
  return association;
}


static void createClass(char **tokens) {
  ObjPtr association;
  ObjPtr *variable;

  if (numberClasses == MAX_NUMBER_CLASSES) {
    error("too many classes");
  }
  classArray[numberClasses].className =
    (char *) malloc(strlen(tokens[2]) + 1);
  if (classArray[numberClasses].className == NULL) {
    error("cannot allocate memory for class name");
  }
  strcpy(classArray[numberClasses].className, tokens[2]);
  association = makeClassAssociation(tokens[2]);
  classArray[numberClasses].classAssociation = association;
  variable = classNeededByInterpreter(tokens[2]);
  if (variable != NULL) {
    /* class needed: store association in machine register */
    *variable = association;
  }
  numberClasses++;
}


static void createClassesFrom(char *fileName) {
  FILE *sourceFile;
  int c;
  char *aString;
  char **tokens;

  printf("Creating classes from %s:\n", fileName);
  sourceFile = fopen(fileName, "rt");
  if (sourceFile == NULL) {
    error("cannot open source file '%s'", fileName);
  }
  while (1) {
    c = getc(sourceFile);
    if (c == EOF) {
      break;
    }
    if (c != '!') {
      ungetc(c, sourceFile);
      aString = nextChunk(sourceFile);
      /* Compiler evaluate: aString */
      tokens = analyzeClassCreationExpression(aString);
      if (tokens == NULL) {
	printf("\n%s\n", aString);
	error("cannot understand");
      }
      printf("    %s\n", tokens[2]);
      createClass(tokens);
    } else {
      aString = nextChunk(sourceFile);
      while (1) {
	aString = nextChunk(sourceFile);
	if (*aString == '\0') {
	  break;
	}
      }
    }
  }
  fclose(sourceFile);
}


/*------------------------------------*/
/* File In                            */
/*------------------------------------*/


#define MAX_NUMBER_INSTVARS		20


static int fillInInstVarNames(ObjPtr class, char *nameString) {
  char *name;
  char *names[MAX_NUMBER_INSTVARS];
  int numberNames, i;
  ObjPtr nameArray;

  numberNames = 0;
  name = strtok(nameString, " \t\n");
  while (name != NULL) {
    if (numberNames == MAX_NUMBER_INSTVARS) {
      error("too many instance variables in class");
    }
    names[numberNames++] = name;
    name = strtok(NULL, " \t\n");
  }
  nameArray = newArray(numberNames);
  for (i = 0; i < numberNames; i++) {
    setPointer(nameArray, i, newString(names[i]));
  }
  setPointer(class, INSTVARS_IN_CLASS, nameArray);
  return numberNames;
}


static void fillInCharacteristic(ObjPtr class,
                                 char *creationKeyword,
                                 int totalNumberInstvars) {
  Word characteristic;

  characteristic = totalNumberInstvars;
  if (strcmp(creationKeyword, "subclass:") == 0) {
    /* set HASPOINTERS flags */
    characteristic |= CLASSCHAR_HASPOINTERS;
  } else
  if (strcmp(creationKeyword, "variableSubclass:") == 0) {
    /* set HASPOINTERS and ISINDEXABLE flags */
    characteristic |= CLASSCHAR_HASPOINTERS;
    characteristic |= CLASSCHAR_ISINDEXABLE;
  } else
  if (strcmp(creationKeyword, "variableBinarySubclass:") == 0) {
    /* set ISINDEXABLE flag */
    characteristic |= CLASSCHAR_ISINDEXABLE;
  } else {
    error("illegal class creation keyword");
  }
  setPointer(class,
	     CHARACTERISTIC_IN_CLASS,
	     newSmallInteger(characteristic));
}


static void fillInClassData(char **tokens) {
  ObjPtr association;
  ObjPtr class;
  ObjPtr metaclass;
  ObjPtr superclass;
  ObjPtr supermeta;
  int numberInstvars;
  int numberSuperInstvars;

  /* get association for class with name in tokens[2] */
  association = lookupClass(tokens[2]);
  /* patch class of association object */
  patchClass(association,
             getPointer(machine.Association, VALUE_IN_ASSOCIATION));
  /* get class */
  class = getPointer(association, VALUE_IN_ASSOCIATION);
  /* get metaclass */
  metaclass = getClass(class);
  /* patch class of metaclass object */
  patchClass(metaclass,
             getPointer(machine.Metaclass, VALUE_IN_ASSOCIATION));
  /* set name in class object */
  setPointer(class, NAME_IN_CLASS, newString(tokens[2]));
  /* set superclass in class object */
  if (strcmp(tokens[0], "nil") == 0) {
    /* superclass of class is nil */
    superclass = machine.nil;
  } else {
    /* superclass of class is not nil */
    superclass = getPointer(lookupClass(tokens[0]), VALUE_IN_ASSOCIATION);
  }
  setPointer(class, SUPERCLASS_IN_CLASS, superclass);
  /* set method dictionary in class object */
  setPointer(class, METHODDICTIONARY_IN_CLASS, newDictionary());
  /* set instance variable names in class object */
  numberInstvars = fillInInstVarNames(class, tokens[3]);
  /* set characteristic in class object */
  if (superclass == machine.nil) {
    /* this is true only for class Object */
    numberSuperInstvars = 0;
  } else {
    /* this is true for all classes except for class Object */
    numberSuperInstvars =
      smallIntegerValue(getPointer(superclass, CHARACTERISTIC_IN_CLASS)) &
      CLASSCHAR_NUMBERMASK;
  }
  fillInCharacteristic(class,
		       tokens[1],
		       numberInstvars + numberSuperInstvars);
  /* set name in metaclass object */
  setPointer(metaclass, NAME_IN_METACLASS, newString(tokens[2]));
  /* set superclass in metaclass object */
  if (superclass == machine.nil) {
    /* superclass of class is nil:
       superclass of metaclass is Class */
    supermeta = getPointer(machine.Class, VALUE_IN_ASSOCIATION);
  } else {
    /* superclass of class is not nil:
       superclass of metaclass is metaclass of superclass */
    supermeta = getClass(getPointer(lookupClass(tokens[0]),
				    VALUE_IN_ASSOCIATION));
  }
  setPointer(metaclass, SUPERCLASS_IN_METACLASS, supermeta);
  /* set method dictionary in metaclass object */
  setPointer(metaclass, METHODDICTIONARY_IN_METACLASS, newDictionary());
  /* set instance variable names in metaclass object */
  numberInstvars = fillInInstVarNames(metaclass, "");
  /* set characteristic in metaclass object */
  if (superclass == machine.nil) {
    numberSuperInstvars = SIZE_OF_CLASS;
  } else {
    numberSuperInstvars =
      smallIntegerValue(getPointer(supermeta, CHARACTERISTIC_IN_CLASS)) &
      CLASSCHAR_NUMBERMASK;
  }
  fillInCharacteristic(metaclass,
		       "subclass:",
		       numberInstvars + numberSuperInstvars);
}


static FILE *sourcesFile;


static void compileForClassInCategory(char *aString, ObjPtr aClass) {
  Word offset;
  Word length;
  ObjPtr associations;
  ObjPtr dictionary;

  /* compile the source string */
  compile(aString, aClass, false);
  /* append source to sources file, record offset and length */
  offset = ftell(sourcesFile);
  fprintf(sourcesFile, "%s\n", aString);
  length = strlen(aString) + 1;
  setPointer(machine.compilerMethod,
	     SOURCEOFFSET_IN_COMPILEDMETHOD,
	     newSmallInteger(offset));
  setPointer(machine.compilerMethod,
	     SOURCELENGTH_IN_COMPILEDMETHOD,
	     newSmallInteger(length));
  /* add the method to the method dictionary of the class */
  associations =
    allocateObject(getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION),
		   SIZE_OF_LINKEDOBJECT,
		   true);
  setPointer(associations,
	     OBJECT_IN_LINKEDOBJECT,
	     machine.compilerAssociation);
  dictionary = getPointer(machine.compilerClass,
			  METHODDICTIONARY_IN_CLASS);
  setPointer(associations,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY));
  setPointer(dictionary,
	     ASSOCIATIONS_IN_DICTIONARY,
	     associations);
}


static void fileInFrom(char **tokens, FILE *sourceFile) {
  char *aString;

  while (1) {
    aString = nextChunk(sourceFile);
    if (*aString == '\0') {
      break;
    }
    if (tokens[1] == NULL) {
      /* Compiler compile: aString
		  forClass: tokens[0]
		  inCategory: tokens[2] */
      compileForClassInCategory(aString,
				getPointer(lookupClass(tokens[0]),
					   VALUE_IN_ASSOCIATION));
    } else {
      /* Compiler compile: aString
		  forClass: tokens[0] class
		  inCategory: tokens[2] */
      compileForClassInCategory(aString,
				getClass(getPointer(lookupClass(tokens[0]),
						    VALUE_IN_ASSOCIATION)));
    }
  }
}


static void fileIn(char *fileName) {
  FILE *sourceFile;
  int c;
  char *aString;
  char **tokens;

  printf("Filing-in %s:\n", fileName);
  sourceFile = fopen(fileName, "rt");
  if (sourceFile == NULL) {
    error("cannot open source file '%s'", fileName);
  }
  while (1) {
    c = getc(sourceFile);
    if (c == EOF) {
      break;
    }
    if (c != '!') {
      ungetc(c, sourceFile);
      aString = nextChunk(sourceFile);
      /* Compiler evaluate: aString */
      tokens = analyzeClassCreationExpression(aString);
      if (tokens == NULL) {
	printf("\n%s\n", aString);
	error("cannot understand");
      }
      printf("    %s\n", tokens[2]);
      fillInClassData(tokens);
    } else {
      aString = nextChunk(sourceFile);
      /* (Compiler evaluate: aString) fileInFrom: sourceFile */
      tokens = analyzeCategoryReaderExpression(aString);
      if (tokens == NULL) {
	printf("\n%s\n", aString);
	error("cannot understand");
      }
      fileInFrom(tokens, sourceFile);
    }
  }
  fclose(sourceFile);
}


/*------------------------------------*/
/* Global System Dictionary Creation  */
/*------------------------------------*/


static void createGlobalSystemDictionary(void) {
  ObjPtr dictionary;
  ObjPtr linkedObject;
  int i;

  /* create the global dictionary */
  machine.MiniTalk =
    allocateObject(getPointer(machine.Association, VALUE_IN_ASSOCIATION),
		   SIZE_OF_ASSOCIATION,
		   true);
  dictionary =
    allocateObject(getPointer(machine.Dictionary, VALUE_IN_ASSOCIATION),
		   SIZE_OF_DICTIONARY,
		   true);
  setPointer(machine.MiniTalk,
	     KEY_IN_ASSOCIATION,
	     newSymbol("MiniTalk"));
  setPointer(machine.MiniTalk,
	     VALUE_IN_ASSOCIATION,
	     dictionary);
  /* it must contain itself */
  linkedObject =
    allocateObject(getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION),
		   SIZE_OF_LINKEDOBJECT,
		   true);
  setPointer(linkedObject,
	     OBJECT_IN_LINKEDOBJECT,
	     machine.MiniTalk);
  setPointer(linkedObject,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY));
  setPointer(dictionary,
	     ASSOCIATIONS_IN_DICTIONARY,
	     linkedObject);
  /* and the global symbol table */
  linkedObject =
    allocateObject(getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION),
		   SIZE_OF_LINKEDOBJECT,
		   true);
  setPointer(linkedObject,
	     OBJECT_IN_LINKEDOBJECT,
	     machine.TheSymbols);
  setPointer(linkedObject,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY));
  setPointer(dictionary,
	     ASSOCIATIONS_IN_DICTIONARY,
	     linkedObject);
  /* and all classes */
  for (i = 0; i < numberClasses; i++) {
    linkedObject =
      allocateObject(getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION),
		     SIZE_OF_LINKEDOBJECT,
		     true);
    setPointer(linkedObject,
	       OBJECT_IN_LINKEDOBJECT,
	       classArray[i].classAssociation);
    setPointer(linkedObject,
	       NEXTLINK_IN_LINKEDOBJECT,
	       getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY));
    setPointer(dictionary,
	       ASSOCIATIONS_IN_DICTIONARY,
	       linkedObject);
  }
}


/*------------------------------------*/
/* Initial Context Creation           */
/*------------------------------------*/


#define INITIAL_CLASS			"Driver"
#define INITIAL_SELECTOR		"start"


static void createInitialContext(void) {
  ObjPtr bytecodes;
  ObjPtr literals;
  ObjPtr method;
  ObjPtr temporaries;
  ObjPtr stack;
  ObjPtr context;

  /*
   * This is the somewhat tricky startup of MiniTalk. The goal is
   * to have as few things fixed as possible. So the actual startup
   * procedure is programmed in MiniTalk itself, only invocation
   * of this procedure is done here (and therefore fixed).
   *
   * We set up a method in execution by installing a MethodContext.
   * The method (which is hand-coded and actually never installed in
   * any class) has the selector 'boot' and pretends to be found in
   * class 'Driver class', so that it could have been invoked by the
   * expression 'Driver boot'. Its definition would look like this:
   *
   * boot
   *    "Boot the Minitalk system."
   *    [true] whileTrue: [self start]
   *
   * The loop is included only as precaution against coding errors in
   * the startup method, which should never return to the boot method.
   * The method with selector 'start' in class 'Driver class' then
   * takes resposibility for actually starting-up MiniTalk.
   *
   * Both names, 'Driver' and 'start', are configurable by defining
   * INITIAL_CLASS and INITIAL_SELECTOR appropriately.
   */
  bytecodes = newByteArray("\x01\x80\x00\x06\xA0\x00\x00", 7);
  literals = newArray(1);
  setPointer(literals, 0, newSymbol(INITIAL_SELECTOR));
  method = newCompiledMethod(newSymbol("boot"),
			     machine.nil,
			     newSmallInteger(0),
			     newSmallInteger(0),
			     newSmallInteger(1),
			     bytecodes,
			     literals);
  temporaries = machine.nil;
  stack = newArray(1);
  context = newMethodContext(machine.nil,
			     newSmallInteger(0),
			     newSmallInteger(0),
			     stack,
			     getClass(getPointer(lookupClass(INITIAL_CLASS),
						 VALUE_IN_ASSOCIATION)),
			     method,
			     getPointer(lookupClass(INITIAL_CLASS),
					VALUE_IN_ASSOCIATION),
			     temporaries);
  machine.currentActiveContext =
    context;
  machine.currentHomeContext =
    context;
  machine.currentSender =
    getPointer(context, SENDER_IN_METHODCONTEXT);
  machine.currentCaller =
    getPointer(context, CALLER_IN_BLOCKCONTEXT);
  machine.currentClass =
    getPointer(context, CLASS_IN_METHODCONTEXT);
  machine.currentMethod =
    getPointer(context, METHOD_IN_METHODCONTEXT);
  machine.currentSelector =
    getPointer(method, SELECTOR_IN_COMPILEDMETHOD);
  machine.currentBytecodes =
    getPointer(method, BYTECODES_IN_COMPILEDMETHOD);
  machine.currentLiterals =
    getPointer(method, LITERALS_IN_COMPILEDMETHOD);
  machine.currentReceiver =
    getPointer(context, RECEIVER_IN_METHODCONTEXT);
  machine.currentTemporaries =
    getPointer(context, TEMPORARIES_IN_METHODCONTEXT);
  machine.currentStack =
    getPointer(context, STACK_IN_METHODCONTEXT);
  machine.ip =
    smallIntegerValue(getPointer(context, INSTPTR_IN_METHODCONTEXT));
  machine.sp =
    smallIntegerValue(getPointer(context, STACKPTR_IN_METHODCONTEXT));
}


/*------------------------------------*/
/* Main Program                       */
/*------------------------------------*/


static void version(char *myself) {
  printf("%s version %d.%d (compiled %s)\n",
         myself, MAJOR_VNUM, MINOR_VNUM, __DATE__);
}


static void help(char *myself) {
  printf("Usage: %s [options] <class list file> [image file]\n", myself);
  printf("Options:\n");
  printf("  --memory         debug memory\n");
  printf("  --scanner        debug scanner\n");
  printf("  --parser         debug parser\n");
  printf("  --tree           show syntax tree\n");
  printf("  --version        show version\n");
  printf("  --help           show this help\n");
}


int main(int argc, char *argv[]) {
  char *classListName;
  char *imageFileName;
  char sourcesFileName[LINE_SIZE];
  int i;

  /* analyze command line */
  classListName = NULL;
  imageFileName = NULL;
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      /* option */
      if (strcmp(argv[i], "--memory") == 0) {
        debugMemory = true;
      } else
      if (strcmp(argv[i], "--scanner") == 0) {
        debugScanner = true;
      } else
      if (strcmp(argv[i], "--parser") == 0) {
        debugParser = true;
      } else
      if (strcmp(argv[i], "--tree") == 0) {
        debugTree = true;
      } else
      if (strcmp(argv[i], "--version") == 0) {
        version(argv[0]);
        exit(0);
      } else
      if (strcmp(argv[i], "--help") == 0) {
        help(argv[0]);
        exit(0);
      } else {
        error("unrecognized option '%s'; try '%s --help'",
              argv[i], argv[0]);
      }
    } else {
      /* file */
      if (imageFileName != NULL) {
        error("more than two file names not allowed");
      }
      if (classListName != NULL) {
        imageFileName = argv[i];
      } else {
        classListName = argv[i];
      }
    }
  }
  if (classListName == NULL) {
    error("no class list file specified");
  }
  if (imageFileName == NULL) {
    imageFileName = STD_IMG_NAME;
  }
  /* construct sources file name from image file name */
  strcpy(sourcesFileName, imageFileName);
  i = strlen(sourcesFileName);
  if (i >= 4 && strcmp(sourcesFileName + i - 4, ".img") == 0) {
    i -= 4;
  }
  strcpy(sourcesFileName + i, ".src");
  /* write greeting message */
  printf("MiniTalk image file generator running on '%s'\n", classListName);
  /* create an empty image file */
  createImageFile(imageFileName);
  /* init object memory */
  initMemory(imageFileName);
  /* create the world */
  bigBangPart1();
  /* create the standard library classes */
  forEachClassDo(classListName, createClassesFrom);
  /* check if all classes that are needed were ever created */
  checkClassesNeededByInterpreter();
  /* patch the world */
  bigBangPart2();
  /* create the global system dictionary */
  createGlobalSystemDictionary();
  /* file-in the standard library classes */
  sourcesFile = fopen(sourcesFileName, "wt");
  if (sourcesFile == NULL) {
    error("cannot open sources file");
  }
  forEachClassDo(classListName, fileIn);
  fclose(sourcesFile);
  /* create the initial context */
  createInitialContext();
  /* exit object memory */
  exitMemory(imageFileName);
  freeClassArray();
  /* write completion message */
  printf("MiniTalk image file generation completed\n");
  /* done */
  return 0;
}
