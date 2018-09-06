
	/**************************************************/
	/**                                              **/
	/**               M i n i T a l k                **/
	/**                                              **/
	/**                 Version 0.3                  **/
	/**                                              **/
	/**                  H. Geisse                   **/
	/**                                              **/
	/**               Fachbereich MNI                **/
	/**                                              **/
	/**       Fachhochschule Giessen-Friedberg       **/
	/**                                              **/
	/**************************************************/


/* choose exactly one of the following compilation models */
/* this can be done by -D switch in compiler command line */

/* #define DOS_STANDARD */
/* #define DOS_EXTENDED */

#ifndef DOS_STANDARD
#ifndef DOS_EXTENDED
#error compilation model undefined
#endif
#endif


/* general messages */

#define MAJOR_VNUM	0
#define MINOR_VNUM	3
#define GREETING	"\nMiniTalk Version %d.%d\n", MAJOR_VNUM, MINOR_VNUM
#define BYEBYE		"MiniTalk session ended, bye!\n"


/* tunable parameters */

#define ORIGINAL_NAME		"original"
#define DEFAULT_NAME		"minitalk"
#define DEFAULT_IMAGE_EXT	"img"
#define DEFAULT_SOURCE_EXT	"mt"
#define STDCLASSES_DIRECTORY	"../cls/"
#define STDCLASSES_SPECFILE	"stdclss.txt"
#define MAX_CHUNKSIZE		4000

#ifdef DOS_STANDARD
#define SEMISIZE		30			/* kilobytes */
#endif
#ifdef DOS_EXTENDED
#define SEMISIZE		500			/* kilobytes */
#endif

#define MEMSIZE			(2 * SEMISIZE)		/* kilobytes */

#define KILO			1024


/* general include files */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* boolean type */

typedef int			Boolean;

#define FALSE			0
#define TRUE			1


/* unsigned types */

typedef unsigned char		Uchar;
typedef unsigned short		Ushort;
typedef unsigned long		Ulong;


/* most significant and next significant bits of Ulong */

#define ULONG_MSB		(((Ulong) 1) << (8 * sizeof(Ulong) - 1))
#define ULONG_NSB		(((Ulong) 1) << (8 * sizeof(Ulong) - 2))


/* two flags are coded in the size field of every object */

#define BROKEN_HEART		ULONG_MSB
#define HAS_POINTERS		ULONG_NSB


/* address types */

typedef Ulong			Address;
typedef Address			ObjPtr;


/* most significant and next significant bits of ObjPtr */

#define OBJPTR_MSB		(((ObjPtr) 1) << (8 * sizeof(ObjPtr) - 1))
#define OBJPTR_NSB		(((ObjPtr) 1) << (8 * sizeof(ObjPtr) - 2))


/* two bits of the object pointer are used in coding small integers */

#define IS_SMALLINT		OBJPTR_MSB
#define IS_NEGATIVE		OBJPTR_NSB


/* virtual machine definition */

typedef struct {
  /* image file structure */
  Ulong memoryStart;		/* byte offset of object memory in file */
  Ulong memorySize;		/* total size of object memory in bytes */
  /* image file version number */
  Ushort majorVersion;		/* = main program's major version */
  Ushort minorVersion;		/* = main program's minor version */
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
  Ushort ip;
  Ushort sp;
  /* registers used by compiler (C version only) */
  ObjPtr compilerClass;
  ObjPtr compilerCode;
  ObjPtr compilerLiteral;
  ObjPtr compilerLiterals;
  ObjPtr compilerMethod;
  ObjPtr compilerAssociation;
} Machine;


/* MINITALK.C */

void error(char *msg);


/* MEMORY.C */

extern Boolean debugMemory;
extern Machine machine;
extern Uchar *memory;

ObjPtr allocateObject(ObjPtr clazz, Ulong size, Boolean hasPointers);
ObjPtr getClass(ObjPtr object);
Ulong getSize(ObjPtr object);
Boolean hasPointers(ObjPtr object);
Uchar *getBytes(ObjPtr object);
Uchar getByte(ObjPtr object, Ulong index);
void setByte(ObjPtr object, Ulong index, Uchar value);
ObjPtr getPointer(ObjPtr object, Ulong index);
void setPointer(ObjPtr object, Ulong index, ObjPtr value);
ObjPtr newSmallInteger(long value);
long smallIntegerValue(ObjPtr smallIntegerObj);
ObjPtr newFloat(double value);
double floatValue(ObjPtr floatObj);
ObjPtr newCharacter(Uchar c);
Uchar characterValue(ObjPtr characterObj);
ObjPtr newString(char *string);
char *stringValue(ObjPtr stringObj);
ObjPtr newSymbol(char *string);
void swapPointers(ObjPtr obj1, ObjPtr obj2);
void initMemory(char *fileName);
void exitMemory(char *fileName);


/* INTRPRTR.C */

extern Boolean debugInterpreter;
extern Boolean run;

#define OP_SPECIALS		0x00
#define OP_PUSHLTRL		0x10
#define OP_PUSHINST		0x20
#define OP_STOREINST		0x30
#define OP_PUSHTEMP		0x40
#define OP_STORETEMP		0x50
#define OP_PUSHASSOC		0x60
#define OP_STOREASSOC		0x70
#define OP_SEND			0x80
#define OP_SENDSUPER		0x90
#define OP_JUMPS		0xA0
#define OP_PUSHBLOCK		0xB0
#define OP_UNUSED_Cx		0xC0
#define OP_UNUSED_Dx		0xD0
#define OP_UNUSED_Ex		0xE0
#define OP_EXTENDED		0xF0

#define OP_NOP			0x00
#define OP_PUSHSELF		0x01
#define OP_PUSHNIL		0x02
#define OP_PUSHFALSE		0x03
#define OP_PUSHTRUE		0x04
#define OP_DUP			0x05
#define OP_POP			0x06
#define OP_RET			0x07
#define OP_RETBLOCK		0x08

#define OP_JUMP			0xA0

void printString(ObjPtr string);
void showObject(char *name, ObjPtr object);
void showInstructions(ObjPtr bytecodes, ObjPtr literals);
void showWhere(ObjPtr class1, ObjPtr class2, ObjPtr selector);
void push(ObjPtr object);
ObjPtr pop(void);
void storeContextRegisters(void);
void fetchContextRegisters(void);
void findMethod(ObjPtr initialClass, ObjPtr selector);
void executeNewMethod(int numArgs);
void runInterpreter(void);


/* PRIMMETH.C */

typedef Boolean (*PrimitiveMethod)(int numberArguments);

extern PrimitiveMethod primitiveMethods[];


/* SCANNER.C */

extern Boolean debugScanner;

#define T_END			0
#define T_IDENT			1
#define T_KEYWORD		2
#define T_KEYWORDS		3
#define T_COLONVAR		4
#define T_BINSEL		5
#define T_ASSIGN		6
#define T_STRING		7
#define T_CHARCON		8
#define T_INTNUM		9
#define T_FLONUM		10
#define T_LPAREN		11
#define T_RPAREN		12
#define T_LBRACK		13
#define T_RBRACK		14
#define T_PERIOD		15
#define T_SEMIC			16
#define T_CARET			17
#define T_HASH			18

extern int tokenType;
extern char stringBuffer[MAX_CHUNKSIZE];
extern char charBuffer;
extern long integerBuffer;
extern double floatBuffer;

void parseError(char *msg);
void initScanner(char *text);
void nextToken(void);


/* TREE.C */

extern Boolean debugTree;

#define V_SELF			0
#define V_SUPER			1
#define V_NIL			2
#define V_FALSE			3
#define V_TRUE			4
#define V_INSTANCE		5
#define V_ARGUMENT		6
#define V_TEMPORARY		7
#define V_SHARED		8

typedef struct variable {
  struct variable *next;
  int type;
  char *name;
  int offset;
} Variable;

#define N_SYMBOL		0
#define N_INTNUM		1
#define N_FLONUM		2
#define N_STRING		3
#define N_CHARCON		4
#define N_ARRAY			5
#define N_VARIABLE		6
#define N_BLOCK			7
#define N_MESSAGE		8
#define N_CASCADE		9
#define N_ASSIGN		10
#define N_RETEXP		11
#define N_METHOD		12

typedef struct node {
  int type;
  union {
    /* N_SYMBOL */
    struct {
      char *name;
    } symbol;
    /* N_INTNUM */
    struct {
      long value;
    } intnum;
    /* N_FLONUM */
    struct {
      double value;
    } flonum;
    /* N_STRING */
    struct {
      char *value;
    } string;
    /* N_CHARCON */
    struct {
      char value;
    } charcon;
    /* N_ARRAY */
    struct {
      struct element *elements;
      int numberElements;
    } array;
    /* N_VARIABLE */
    struct {
      struct variable *record;
    } variable;
    /* N_BLOCK */
    struct {
      struct element *variables;
      int numberVariables;
      struct element *statements;
    } block;
    /* N_MESSAGE */
    struct {
      struct node *receiver;
      Boolean superFlag;
      struct node *selector;
      struct element *arguments;
    } message;
    /* N_CASCADE */
    struct {
      struct node *receiver;
      struct element *messages;
    } cascade;
    /* N_ASSIGN */
    struct {
      struct element *variables;
      struct node *expression;
    } assign;
    /* N_RETEXP */
    struct {
      struct node *expression;
    } retexp;
    /* N_METHOD */
    struct {
      struct node *selector;
      int numberArguments;
      int numberTemporaries;
      int primitive;
      struct element *statements;
    } method;
  } all;
} Node;

typedef struct element {
  struct element *next;
  struct node *element;
} Element;

char *copyString(char *string);
char *appendString(char *alreadyThere, char *toBeAppended);

Boolean isAssignable(Variable *variable);
Variable *lookupVariable(char *name, Variable **variables);
Variable *enterVariable(int type, char *name, Variable **variables);
void initVariables(Variable **variables, ObjPtr aClass);
void computeOffsets(Variable **variables, Node *method);
void showVariables(Variable **variables);
void freeVariables(Variable **variables);

Node *newNode(int type);
Element *appendElement(Element *list, Node *element);
void showTree(Node *tree);
void freeTree(Node *tree);


/* PARSER.C */

extern Boolean debugParser;

Node *parseMethod(Variable **variables);


/* COMPILER.C */

void compile(char *aString, ObjPtr aClass, Boolean valueNeeded);


/* GRAPHICS.C */

void graphicsOn(void);
void graphicsOff(void);
void graphicsLine(int x1, int y1, int x2, int y2, int color);
void graphicsRectangle(int x1, int y1, int x2, int y2, int color);


/* some classes have a structure which is known by the interpreter */

#define CLASSCHAR_HASPOINTERS		0x8000
#define CLASSCHAR_ISINDEXABLE		0x4000
#define CLASSCHAR_NUMBERMASK		0x00FF

#define SUPERCLASS_IN_METACLASS		0
#define METHODDICTIONARY_IN_METACLASS	1
#define CHARACTERISTIC_IN_METACLASS	2
#define NAME_IN_METACLASS		3
#define INSTVARS_IN_METACLASS		4
#define SIZE_OF_METACLASS		5

#define SUPERCLASS_IN_CLASS		0
#define METHODDICTIONARY_IN_CLASS	1
#define CHARACTERISTIC_IN_CLASS		2
#define NAME_IN_CLASS			3
#define INSTVARS_IN_CLASS		4
#define SIZE_OF_CLASS			5

#define SELECTOR_IN_COMPILEDMETHOD	0
#define PRIMITIVE_IN_COMPILEDMETHOD	1
#define NUMBERARGS_IN_COMPILEDMETHOD	2
#define TEMPSIZE_IN_COMPILEDMETHOD	3
#define STACKSIZE_IN_COMPILEDMETHOD	4
#define BYTECODES_IN_COMPILEDMETHOD	5
#define LITERALS_IN_COMPILEDMETHOD	6
#define SOURCEOFFSET_IN_COMPILEDMETHOD	7
#define SOURCELENGTH_IN_COMPILEDMETHOD	8
#define SIZE_OF_COMPILEDMETHOD		9

#define INSTPTR_IN_CONTEXT		1
#define STACKPTR_IN_CONTEXT		2
#define STACK_IN_CONTEXT		3

#define SENDER_IN_METHODCONTEXT		0
#define INSTPTR_IN_METHODCONTEXT	INSTPTR_IN_CONTEXT
#define STACKPTR_IN_METHODCONTEXT	STACKPTR_IN_CONTEXT
#define STACK_IN_METHODCONTEXT		STACK_IN_CONTEXT
#define CLASS_IN_METHODCONTEXT		4
#define METHOD_IN_METHODCONTEXT		5
#define RECEIVER_IN_METHODCONTEXT	6
#define TEMPORARIES_IN_METHODCONTEXT	7
#define SIZE_OF_METHODCONTEXT		8

#define CALLER_IN_BLOCKCONTEXT		0
#define INSTPTR_IN_BLOCKCONTEXT		INSTPTR_IN_CONTEXT
#define STACKPTR_IN_BLOCKCONTEXT	STACKPTR_IN_CONTEXT
#define STACK_IN_BLOCKCONTEXT		STACK_IN_CONTEXT
#define NUMBERARGS_IN_BLOCKCONTEXT	4
#define INITIALIP_IN_BLOCKCONTEXT	5
#define HOME_IN_BLOCKCONTEXT		6
#define SIZE_OF_BLOCKCONTEXT		7

#define KEY_IN_ASSOCIATION		0
#define VALUE_IN_ASSOCIATION		1
#define SIZE_OF_ASSOCIATION		2

#define NEXTLINK_IN_LINKEDOBJECT	0
#define OBJECT_IN_LINKEDOBJECT		1
#define SIZE_OF_LINKEDOBJECT		2

#define ASSOCIATIONS_IN_DICTIONARY	0
#define SIZE_OF_DICTIONARY		1
