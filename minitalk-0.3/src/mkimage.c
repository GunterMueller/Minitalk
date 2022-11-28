
/*
 * MKIMAGE.C	image file creator for MiniTalk
 */


#include "minitalk.h"


#ifdef DOS_STANDARD
#include <alloc.h>	/* to get prototypes for coreleft(), farcoreleft() */
#endif


/*------------------------------------*/
/* Object Generation And Access       */
/*------------------------------------*/


ObjPtr newArray(Ulong size)
{
  ObjPtr array;

  array =
    allocateObject(getPointer(machine.Array, VALUE_IN_ASSOCIATION),
		   size,
		   TRUE);
  return array;
}


ObjPtr newByteArray(Uchar *bytes, Ulong size)
{
  ObjPtr byteArray;

  byteArray =
    allocateObject(getPointer(machine.ByteArray, VALUE_IN_ASSOCIATION),
		   size,
		   FALSE);
  memcpy(getBytes(byteArray), bytes, size);
  return byteArray;
}


ObjPtr newCompiledMethod(ObjPtr selector,
			 ObjPtr primitive,
			 ObjPtr numberargs,
			 ObjPtr tempsize,
			 ObjPtr stacksize,
			 ObjPtr bytecodes,
			 ObjPtr literals)
{
  ObjPtr compiledMethod;

  compiledMethod =
    allocateObject(getPointer(machine.CompiledMethod, VALUE_IN_ASSOCIATION),
		   SIZE_OF_COMPILEDMETHOD,
		   TRUE);
  setPointer(compiledMethod, SELECTOR_IN_COMPILEDMETHOD, selector);
  setPointer(compiledMethod, PRIMITIVE_IN_COMPILEDMETHOD, primitive);
  setPointer(compiledMethod, NUMBERARGS_IN_COMPILEDMETHOD, numberargs);
  setPointer(compiledMethod, TEMPSIZE_IN_COMPILEDMETHOD, tempsize);
  setPointer(compiledMethod, STACKSIZE_IN_COMPILEDMETHOD, stacksize);
  setPointer(compiledMethod, BYTECODES_IN_COMPILEDMETHOD, bytecodes);
  setPointer(compiledMethod, LITERALS_IN_COMPILEDMETHOD, literals);
  return compiledMethod;
}


ObjPtr newMethodContext(ObjPtr sender,
			ObjPtr instptr,
			ObjPtr stackptr,
			ObjPtr stack,
			ObjPtr clazz,
			ObjPtr method,
			ObjPtr receiver,
			ObjPtr temporaries)
{
  ObjPtr context;

  context =
    allocateObject(getPointer(machine.MethodContext, VALUE_IN_ASSOCIATION),
		   SIZE_OF_METHODCONTEXT,
		   TRUE);
  setPointer(context, SENDER_IN_METHODCONTEXT, sender);
  setPointer(context, INSTPTR_IN_METHODCONTEXT, instptr);
  setPointer(context, STACKPTR_IN_METHODCONTEXT, stackptr);
  setPointer(context, STACK_IN_METHODCONTEXT, stack);
  setPointer(context, CLASS_IN_METHODCONTEXT, clazz);
  setPointer(context, METHOD_IN_METHODCONTEXT, method);
  setPointer(context, RECEIVER_IN_METHODCONTEXT, receiver);
  setPointer(context, TEMPORARIES_IN_METHODCONTEXT, temporaries);
  return context;
}


ObjPtr newDictionary(void)
{
  ObjPtr dictionary;

  dictionary =
    allocateObject(getPointer(machine.Dictionary, VALUE_IN_ASSOCIATION),
		   SIZE_OF_DICTIONARY,
		   TRUE);
  return dictionary;
}


/*------------------------------------*/
/* Standard Classes                   */
/*------------------------------------*/


#define LINESIZE		80


Boolean isLowerCaseVowel(char c)
{
  return c == 'a' ||
	 c == 'e' ||
	 c == 'i' ||
	 c == 'o' ||
	 c == 'u';
}


void strlwr(char *str)
{
  while (*str != '\0') {
    if (*str >= 'A' && *str <= 'Z') {
      *str += 'a' - 'A';
    }
    str++;
  }
}


char *makeFilenameFromClassname(char *className)
{
  static char fileName[80];
  int length;
  char *revPtr;

  strcpy(fileName, className);
  length = strlen(fileName);
  revPtr = fileName + length - 1;
  while (length > 8) {
    if (isLowerCaseVowel(*revPtr)) {
      strcpy(revPtr, revPtr + 1);
      length--;
    }
    if (revPtr == fileName) {
      break;
    }
    revPtr--;
  }
  if (length > 8) {
    fileName[8] = '\0';
  }
  strlwr(fileName);
  return fileName;
}


void forEachStdClassDo(void function(char *fileName))
{
  char specFileName[50];
  FILE *stdClassesFile;
  char line[LINESIZE];
  char *cp, *className;
  char sourceFileName[50];

  /* open file with names of standard classes */
  strcpy(specFileName, STDCLASSES_DIRECTORY);
  strcat(specFileName, STDCLASSES_SPECFILE);
  stdClassesFile = fopen(specFileName, "rt");
  if (stdClassesFile == NULL) {
    error("cannot open file with names of standard classes");
  }
  /* work on file line by line */
  while (fgets(line, LINESIZE, stdClassesFile) != NULL) {
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
    strcpy(sourceFileName, STDCLASSES_DIRECTORY);
    strcat(sourceFileName, makeFilenameFromClassname(className));
    strcat(sourceFileName, ".");
    strcat(sourceFileName, DEFAULT_SOURCE_EXT);
    /* apply function to filename */
    function(sourceFileName);
  }
  /* close file with names of standard classes */
  fclose(stdClassesFile);
}


/*------------------------------------*/
/* Error Handling                     */
/*------------------------------------*/


void error(char *msg)
{
  printf("\n**** ERROR: %s ****\n", msg);
  exit(2);
}


/*------------------------------------*/
/* Image File Creation                */
/*------------------------------------*/


void createImageFile(char *fileName)
{
  FILE *imageFile;

  /* try to create image file */
  imageFile = fopen(fileName, "wb");
  if (imageFile == NULL) {
    error("cannot create image file");
  }
  /* write initial machine state */
  machine.memoryStart = sizeof(Machine);
  machine.memorySize = 0;
  machine.majorVersion = MAJOR_VNUM;
  machine.minorVersion = MINOR_VNUM;
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


void bigBangPart1(void)
{
  int i;
  ObjPtr symbol;
  ObjPtr symbols;

  /* create nil: this will allow to work alloacteObject() correctly */
  machine.nil = allocateObject((ObjPtr) 0, 0, FALSE);
  /* now false and true can be created */
  machine.false = allocateObject(machine.nil, 0, FALSE);
  machine.true = allocateObject(machine.nil, 0, FALSE);
  /* then, the characters */
  for (i = 0; i < 256; i++) {
    machine.character[i] = allocateObject(machine.nil, 1, FALSE);
    setByte(machine.character[i], 0, i);
  }
  /* finally, the symbols */
  symbol = allocateObject(machine.nil, strlen(THE_SYMBOLS), FALSE);
  strncpy(getBytes(symbol), THE_SYMBOLS, strlen(THE_SYMBOLS));
  symbols = allocateObject(machine.nil, SIZE_OF_LINKEDOBJECT, TRUE);
  setPointer(symbols, OBJECT_IN_LINKEDOBJECT, symbol);
  machine.TheSymbols = allocateObject(machine.nil, SIZE_OF_ASSOCIATION, TRUE);
  setPointer(machine.TheSymbols, KEY_IN_ASSOCIATION, symbol);
  setPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION, symbols);
}


void bigBangPart2(void)
{
  int i;
  ObjPtr symbols;
  ObjPtr symbol;

  /* patch classes of objects created in part 1 */
  * (ObjPtr *) (memory + machine.nil) =
    getPointer(machine.UndefinedObject, VALUE_IN_ASSOCIATION);
  * (ObjPtr *) (memory + machine.false) =
    getPointer(machine.False, VALUE_IN_ASSOCIATION);
  * (ObjPtr *) (memory + machine.true) =
    getPointer(machine.True, VALUE_IN_ASSOCIATION);
  for (i = 0; i < 256; i++) {
    * (ObjPtr *) (memory + machine.character[i]) =
      getPointer(machine.Character, VALUE_IN_ASSOCIATION);
  }
  * (ObjPtr *) (memory + machine.TheSymbols) =
    getPointer(machine.Association, VALUE_IN_ASSOCIATION);
  symbols = getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION);
  while (symbols != machine.nil) {
    * (ObjPtr *) (memory + symbols) =
      getPointer(machine.LinkedObject, VALUE_IN_ASSOCIATION);
    symbol = getPointer(symbols, OBJECT_IN_LINKEDOBJECT);
    * (ObjPtr *) (memory + symbol) =
      getPointer(machine.Symbol, VALUE_IN_ASSOCIATION);
    symbols = getPointer(symbols, NEXTLINK_IN_LINKEDOBJECT);
  }
}


/*------------------------------------*/
/* Chunk Handling                     */
/*------------------------------------*/


char *nextChunk(FILE *sourceFile)
{
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


char **analyzeClassCreationExpression(char *aString)
{
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


char **analyzeCategoryReaderExpression(char *aString)
{
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
  Boolean initialized;
} classesNeededByInterpreter[] = {
  { "UndefinedObject", &machine.UndefinedObject, FALSE },
  { "False",           &machine.False,           FALSE },
  { "True",            &machine.True,            FALSE },
  { "Character",       &machine.Character,       FALSE },
  { "SmallInteger",    &machine.SmallInteger,    FALSE },
  { "Float",           &machine.Float,           FALSE },
  { "LinkedObject",    &machine.LinkedObject,    FALSE },
  { "Association",     &machine.Association,     FALSE },
  { "Dictionary",      &machine.Dictionary,      FALSE },
  { "Array",           &machine.Array,           FALSE },
  { "ByteArray",       &machine.ByteArray,       FALSE },
  { "String",          &machine.String,          FALSE },
  { "Symbol",          &machine.Symbol,          FALSE },
  { "CompiledMethod",  &machine.CompiledMethod,  FALSE },
  { "BlockContext",    &machine.BlockContext,    FALSE },
  { "MethodContext",   &machine.MethodContext,   FALSE },
  { "Class",           &machine.Class,           FALSE },
  { "Metaclass",       &machine.Metaclass,       FALSE }
};


ObjPtr *classNeededByInterpreter(char *className)
{
  int i;

  for (i = 0; i < sizeof(classesNeededByInterpreter) /
		  sizeof(classesNeededByInterpreter[0]); i++) {
    if (strcmp(classesNeededByInterpreter[i].name, className) == 0) {
      classesNeededByInterpreter[i].initialized = TRUE;
      return classesNeededByInterpreter[i].variable;
    }
  }
  return NULL;
}


void checkClassesNeededByInterpreter(void)
{
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


int numberClasses = 0;


struct classStruct {
  char *className;
  ObjPtr classAssociation;
} classArray[MAX_NUMBER_CLASSES];


void freeClassArray(void)
{
  int i;

  for (i = 0; i < numberClasses; i++) {
    free(classArray[i].className);
  }
}


ObjPtr lookupClass(char *className)
{
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


ObjPtr makeClassAssociation(char *className)
{
  ObjPtr metaclass;
  ObjPtr clazz;
  ObjPtr association;
  ObjPtr key;
  ObjPtr symbols;

  metaclass = allocateObject(machine.nil, SIZE_OF_METACLASS, TRUE);
  clazz = allocateObject(metaclass, SIZE_OF_CLASS, TRUE);
  association = allocateObject(machine.nil, SIZE_OF_ASSOCIATION, TRUE);
  setPointer(association, VALUE_IN_ASSOCIATION, clazz);
  key = allocateObject(machine.nil, strlen(className), FALSE);
  strncpy(getBytes(key), className, strlen(className));
  setPointer(association, KEY_IN_ASSOCIATION, key);
  symbols = allocateObject(machine.nil, SIZE_OF_LINKEDOBJECT, TRUE);
  setPointer(symbols, OBJECT_IN_LINKEDOBJECT, key);
  setPointer(symbols,
	     NEXTLINK_IN_LINKEDOBJECT,
	     getPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION));
  setPointer(machine.TheSymbols, VALUE_IN_ASSOCIATION, symbols);
  return association;
}


void createClass(char **tokens)
{
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


void createClassesFrom(char *fileName)
{
  FILE *sourceFile;
  int c;
  char *aString;
  char **tokens;

  printf("Creating classes from %s: ", fileName);
  sourceFile = fopen(fileName, "rt");
  if (sourceFile == NULL) {
    error("cannot open source file");
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
      printf("%s ", tokens[2]);
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
  printf("\n");
}


/*------------------------------------*/
/* File In                            */
/*------------------------------------*/


#define MAX_NUMBER_INSTVARS		20


int fillInInstVarNames(ObjPtr clazz, char *nameString)
{
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
  setPointer(clazz, INSTVARS_IN_CLASS, nameArray);
  return numberNames;
}


void fillInCharacteristic(ObjPtr clazz,
			  char *creationKeyword,
			  int totalNumberInstvars)
{
  Ulong characteristic;

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
  setPointer(clazz,
	     CHARACTERISTIC_IN_CLASS,
	     newSmallInteger(characteristic));
}


void fillInClassData(char **tokens)
{
  ObjPtr association;
  ObjPtr clazz;
  ObjPtr metaclass;
  ObjPtr superclass;
  ObjPtr supermeta;
  int numberInstvars;
  int numberSuperInstvars;

  /* get association for class with name in tokens[2] */
  association = lookupClass(tokens[2]);
  /* patch class of association object */
  * (ObjPtr *) (memory + association) =
    getPointer(machine.Association, VALUE_IN_ASSOCIATION);
  /* get class */
  clazz = getPointer(association, VALUE_IN_ASSOCIATION);
  /* get metaclass */
  metaclass = getClass(clazz);
  /* patch class of metaclass object */
  * (ObjPtr *) (memory + metaclass) =
    getPointer(machine.Metaclass, VALUE_IN_ASSOCIATION);
  /* set name in class object */
  setPointer(clazz, NAME_IN_CLASS, newString(tokens[2]));
  /* set superclass in class object */
  if (strcmp(tokens[0], "nil") == 0) {
    /* superclass of class is nil */
    superclass = machine.nil;
  } else {
    /* superclass of class is not nil */
    superclass = getPointer(lookupClass(tokens[0]), VALUE_IN_ASSOCIATION);
  }
  setPointer(clazz, SUPERCLASS_IN_CLASS, superclass);
  /* set method dictionary in class object */
  setPointer(clazz, METHODDICTIONARY_IN_CLASS, newDictionary());
  /* set instance variable names in class object */
  numberInstvars = fillInInstVarNames(clazz, tokens[3]);
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
  fillInCharacteristic(clazz,
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


FILE *sourcesFile;


void compileForClassInCategory(char *aString, ObjPtr aClass)
{
  Ulong offset;
  Ulong length;
  ObjPtr associations;
  ObjPtr dictionary;

  /* compile the source string */
  compile(aString, aClass, FALSE);
  /* append source to sources file, record offset and length */
  offset = ftell(sourcesFile);
  fprintf(sourcesFile, "%s\n", aString);
  /* ATTENTION: do not compute the length according to
	 length = ftell(sourcesFile) - offset;
     because this counts line breaks as 2 characters,
     even if the file is opened in text mode, i.e. it
     counts the bytes in the file, not in the string */
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
		   TRUE);
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


void fileInFrom(char **tokens, FILE *sourceFile)
{
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


void fileIn(char *fileName)
{
  FILE *sourceFile;
  int c;
  char *aString;
  char **tokens;

  printf("Filing-in %s: ", fileName);
  sourceFile = fopen(fileName, "rt");
  if (sourceFile == NULL) {
    error("cannot open source file");
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
      printf("%s ", tokens[2]);
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
  printf("\n");
}


/*------------------------------------*/
/* Global System Dictionary Creation  */
/*------------------------------------*/


void createGlobalSystemDictionary(void)
{
  ObjPtr dictionary;
  ObjPtr linkedObject;
  int i;

  /* create the global dictionary */
  machine.MiniTalk =
    allocateObject(getPointer(machine.Association, VALUE_IN_ASSOCIATION),
		   SIZE_OF_ASSOCIATION,
		   TRUE);
  dictionary =
    allocateObject(getPointer(machine.Dictionary, VALUE_IN_ASSOCIATION),
		   SIZE_OF_DICTIONARY,
		   TRUE);
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
		   TRUE);
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
		   TRUE);
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
		     TRUE);
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


void createInitialContext(void)
{
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


void usage(void)
{
  printf("\nUsage: mkimage [-dm] [-ds] [-dp] [-dt] [file]\n");
  exit(1);
}


int main(int argc, char *argv[])
{
  int i;
  char *argp;
  char *fileName;
  char imageFileName[50];
  char sourcesFileName[50];
#ifdef DOS_STANDARD
  unsigned long nearCoreBefore, nearCoreAfter;
  unsigned long farCoreBefore, farCoreAfter;
#endif

  /* analyze command line arguments */
  fileName = NULL;
  for (i = 1; i < argc; i++) {
    argp = argv[i];
    if (*argp == '-') {
      /* option */
      argp++;
      if (strcmp(argp, "dm") == 0) {
	debugMemory = TRUE;
      } else
      if (strcmp(argp, "ds") == 0) {
	debugScanner = TRUE;
      } else
      if (strcmp(argp, "dp") == 0) {
	debugParser = TRUE;
      } else
      if (strcmp(argp, "dt") == 0) {
	debugTree = TRUE;
      } else {
	usage();
      }
    } else {
      /* file */
      if (fileName == NULL) {
	fileName = argp;
      } else {
	usage();
      }
    }
  }
  /* set image and sources file names */
  if (fileName == NULL) {
    strcpy(imageFileName, ORIGINAL_NAME);
    strcpy(sourcesFileName, ORIGINAL_NAME);
  } else {
    strcpy(imageFileName, fileName);
    strcpy(sourcesFileName, fileName);
  }
  if (strchr(imageFileName, '.') == NULL) {
    strcat(imageFileName, ".");
    strcat(imageFileName, DEFAULT_IMAGE_EXT);
    strcat(sourcesFileName, ".");
    strcat(sourcesFileName, DEFAULT_SOURCE_EXT);
  } else {
    error("file name must not contain extension");
  }
  /* write greeting */
  printf("\nMKIMAGE - Image File Creator for MiniTalk\n");
  /* create an empty image file */
  createImageFile(imageFileName);
  /* init object memory */
#ifdef DOS_STANDARD
  nearCoreBefore = coreleft();
  farCoreBefore = farcoreleft();
#endif
  initMemory(imageFileName);
  /* create the world */
  bigBangPart1();
  /* create the standard library classes */
  forEachStdClassDo(createClassesFrom);
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
  forEachStdClassDo(fileIn);
  fclose(sourcesFile);
  /* create the initial context */
  createInitialContext();
  /* exit object memory */
  exitMemory(imageFileName);
  freeClassArray();
#ifdef DOS_STANDARD
  nearCoreAfter = coreleft();
  farCoreAfter = farcoreleft();
  if (nearCoreBefore != nearCoreAfter) {
    printf("near core: %lu vs. %lu", nearCoreBefore, nearCoreAfter);
    error("memory leakage");
  }
  if (farCoreBefore != farCoreAfter) {
    printf("far core: %lu vs. %lu", farCoreBefore, farCoreAfter);
    error("memory leakage");
  }
#endif
  /* write completion message */
  printf("Image file creation completed\n");
  /* return to OS */
  return 0;
}
