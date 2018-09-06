
/*
 * INTRPRTR.C	bytecode interpreter
 */


#include "minitalk.h"


#define HI4(x)		((x) >> 4)
#define LO4(x)		((x) & 0x0F)


Boolean debugInterpreter = FALSE;


Boolean run;


/*------------------------------------*/
/* String Output                      */
/*------------------------------------*/


void printString(ObjPtr string)
{
  Ulong size;
  Uchar *cp;

  size = getSize(string);
  cp = getBytes(string);
  while (size--) {
    putchar(*cp++);
  }
}


/*------------------------------------*/
/* Object Inspector                   */
/*------------------------------------*/


char *article(char firstCharOfNoun)
{
  static char *an = "an";
  static char *a = "a";

  if (firstCharOfNoun == 'A' || firstCharOfNoun == 'a' ||
      firstCharOfNoun == 'E' || firstCharOfNoun == 'e' ||
      firstCharOfNoun == 'I' || firstCharOfNoun == 'i' ||
      firstCharOfNoun == 'O' || firstCharOfNoun == 'o' ||
      firstCharOfNoun == 'U' || firstCharOfNoun == 'u') {
    return an;
  } else {
    return a;
  }
}


void showObject(char *name, ObjPtr object)
{
  ObjPtr clazz, className;
  Ulong size, i;
  Boolean pointers;
  ObjPtr pointer;
  Uchar byte;

  /* show name */
  if (name != NULL) {
    printf("%s: ", name);
  }
  /* show object pointer and class */
  clazz = getClass(object);
  className = getPointer(clazz, NAME_IN_CLASS);
  printf("object with pointer 0x%08lX is %s ",
	 object, article(getByte(className, 0)));
  printString(className);
  if (getClass(clazz) ==
      getPointer(machine.Metaclass, VALUE_IN_ASSOCIATION)) {
    printf(" class");
  }
  printf(" (class 0x%08lX)\n", clazz);
  /* show size and type */
  size = getSize(object);
  pointers = hasPointers(object);
  printf("    size is %lu ", size);
  if (pointers) {
    printf("pointers");
  } else {
    printf("bytes");
  }
  printf("\n");
  /* show contents */
  for (i = 0; i < size; i++) {
    if (pointers) {
      pointer = getPointer(object, i);
      clazz = getClass(pointer);
      className = getPointer(clazz, NAME_IN_CLASS);
      printf("    pointer %lu: 0x%08lX (%s ",
	     i, pointer, article(getByte(className, 0)));
      printString(className);
      if (getClass(clazz) ==
	  getPointer(machine.Metaclass, VALUE_IN_ASSOCIATION)) {
	printf(" class");
      }
      printf(")\n");
    } else {
      byte = getByte(object, i);
      printf("    byte %lu: 0x%02X ('%c')\n",
	     i, byte, byte);
    }
  }
}


/*------------------------------------*/
/* Debugger                           */
/*------------------------------------*/


int bytesOutput;


void resetOutput(void)
{
  bytesOutput = 0;
}


void alignOutput(void)
{
  int i;

  i = 4 - bytesOutput;
  while (i--) {
    printf("   ");
  }
}


void showByte(Uchar byte)
{
  printf("%02X ", byte);
  bytesOutput++;
}


void showOffset(Ushort offset)
{
  showByte(offset >> 8);
  showByte(offset & 0xFF);
}


void showLiteral(ObjPtr literals, int litnum)
{
  ObjPtr literal;
  ObjPtr className;

  literal = getPointer(literals, litnum);
  className = getPointer(getClass(literal), NAME_IN_CLASS);
  printf("\t\t; 0x%08lX (%s ", literal, article(getByte(className, 0)));
  printString(className);
  printf(")");
}


Ushort showInstruction(Ushort ip,
		       ObjPtr bytecodes,
		       ObjPtr literals)
{
  Uchar opcode, value;
  Uchar selectorNumber;
  Uchar stacksize;
  Ushort offset;

  printf("%04X\t", ip);
  resetOutput();
  opcode = getByte(bytecodes, ip++);
  showByte(opcode);
  value = LO4(opcode);
  opcode = HI4(opcode);
  if (opcode == HI4(OP_EXTENDED)) {
    opcode = value;
    value = getByte(bytecodes, ip++);
    showByte(value);
  }
  switch (opcode) {
    case HI4(OP_SPECIALS):
	alignOutput();
	switch (value) {
	  case LO4(OP_NOP):
		printf("NOP");
		break;
	  case LO4(OP_PUSHSELF):
		printf("PUSHSELF");
		break;
	  case LO4(OP_PUSHNIL):
		printf("PUSHNIL");
		break;
	  case LO4(OP_PUSHFALSE):
		printf("PUSHFALSE");
		break;
	  case LO4(OP_PUSHTRUE):
		printf("PUSHTRUE");
		break;
	  case LO4(OP_DUP):
		printf("DUP");
		break;
	  case LO4(OP_POP):
		printf("POP");
		break;
	  case LO4(OP_RET):
		printf("RET");
		break;
	  case LO4(OP_RETBLOCK):
		printf("RETBLOCK");
		break;
	  default:
		error("illegal special bytecode encountered");
		break;
	}
	break;
    case HI4(OP_PUSHLTRL):
	alignOutput();
	printf("PUSHLTRL\t%d", value);
	showLiteral(literals, value);
	break;
    case HI4(OP_PUSHINST):
	alignOutput();
	printf("PUSHINST\t%d", value);
	break;
    case HI4(OP_STOREINST):
	alignOutput();
	printf("STOREINST\t%d", value);
	break;
    case HI4(OP_PUSHTEMP):
	alignOutput();
	printf("PUSHTEMP\t%d", value);
	break;
    case HI4(OP_STORETEMP):
	alignOutput();
	printf("STORETEMP\t%d", value);
	break;
    case HI4(OP_PUSHASSOC):
	alignOutput();
	printf("PUSHASSOC\t%d", value);
	showLiteral(literals, value);
	break;
    case HI4(OP_STOREASSOC):
	alignOutput();
	printf("STOREASSOC\t%d", value);
	showLiteral(literals, value);
	break;
    case HI4(OP_SEND):
	selectorNumber = getByte(bytecodes, ip++);
	showByte(selectorNumber);
	alignOutput();
	printf("SEND\t%d, %d", value, selectorNumber);
	showLiteral(literals, selectorNumber);
	break;
    case HI4(OP_SENDSUPER):
	selectorNumber = getByte(bytecodes, ip++);
	showByte(selectorNumber);
	alignOutput();
	printf("SENDSUPER\t%d, %d", value, selectorNumber);
	showLiteral(literals, selectorNumber);
	break;
    case HI4(OP_JUMPS):
	offset = getByte(bytecodes, ip++);
	offset <<= 8;
	offset |= getByte(bytecodes, ip++);
	showOffset(offset);
	alignOutput();
	switch (value) {
	  case LO4(OP_JUMP):
		printf("JUMP");
		break;
	  default:
		error("illegal jump bytecode encountered");
		break;
	}
	printf("\t0x%04X", offset);
	break;
    case HI4(OP_PUSHBLOCK):
	stacksize = getByte(bytecodes, ip++);
	showByte(stacksize);
	alignOutput();
	printf("PUSHBLOCK\t%d, %d", value, stacksize);
	break;
    case HI4(OP_UNUSED_Cx):
	error("illegal bytecode UNUSED_Cx encountered");
	break;
    case HI4(OP_UNUSED_Dx):
	error("illegal bytecode UNUSED_Dx encountered");
	break;
    case HI4(OP_UNUSED_Ex):
	error("illegal bytecode UNUSED_Ex encountered");
	break;
    case HI4(OP_EXTENDED):
	error("illegal bytecode EXTENDED encountered");
	break;
  }
  printf("\n");
  return ip;
}


void showInstructions(ObjPtr bytecodes, ObjPtr literals)
{
  Ushort size, ip;

  size = getSize(bytecodes);
  ip = 0;
  while (ip < size) {
    ip = showInstruction(ip, bytecodes, literals);
  }
}


void showWhere(ObjPtr class1, ObjPtr class2, ObjPtr selector)
{
  printString(getPointer(class1, NAME_IN_CLASS));
  if (class1 != class2) {
    printf(" (");
    printString(getPointer(class2, NAME_IN_CLASS));
    printf(")");
  }
  printf(" >> ");
  printString(selector);
  printf("\n");
}


void debug(void)
{
  Boolean back;
  char c;
  ObjPtr object;

  back = FALSE;
  while (!back) {
    showInstruction(machine.ip,
		    machine.currentBytecodes,
		    machine.currentLiterals);
    do {
      printf("DEBUG: inspect, where, list, next, go, quit? ");
      fflush(stdout);
      do {
        c = getchar();
      } while (c == '\n' || c == '\r') ;
      printf("\r");
      printf("                                             ");
      printf("\r");
    } while (c != 'i' && c != 'w' && c != 'l' &&
	     c != 'n' && c != 'g' && c != 'q') ;
    switch (c) {
      case 'i':
	printf("INSPECT: activeCtx, homeCtx, fromCtx, "
	       "method, receiver, temps, stack, other? ");
	fflush(stdout);
	do {
	  c = getchar();
	} while (c == '\n' || c == '\r') ;
	printf("\r");
	printf("                                     "
	       "                                       ");
	printf("\r");
	switch (c) {
	  case 'a':
		showObject("activeCtx", machine.currentActiveContext);
		break;
	  case 'h':
		if (machine.currentHomeContext !=
		    machine.currentActiveContext) {
		  showObject("homeCtx", machine.currentHomeContext);
		} else {
		  printf("homeContext == activeContext\n");
		}
		break;
	  case 'f':
		showObject("sender", machine.currentSender);
		if (machine.currentHomeContext !=
		    machine.currentActiveContext) {
		  showObject("caller", machine.currentCaller);
		}
		break;
	  case 'm':
		showObject("method", machine.currentMethod);
		break;
	  case 'r':
		showObject("receiver", machine.currentReceiver);
		break;
	  case 't':
		showObject("temps", machine.currentTemporaries);
		break;
	  case 's':
		printf("stackptr = %u\n", machine.sp);
		showObject("stack", machine.currentStack);
		break;
	  case 'o':
		printf("object pointer? 0x");
		scanf("%lx", &object);
		showObject(NULL, object);
		break;
	}
	break;
      case 'w':
	/* nothing to do here */
	break;
      case 'l':
	showWhere(getClass(machine.currentReceiver),
		  machine.currentClass,
		  machine.currentSelector);
	showInstructions(machine.currentBytecodes,
			 machine.currentLiterals);
	break;
      case 'n':
	back = TRUE;
	break;
      case 'g':
	debugInterpreter = FALSE;
	back = TRUE;
	break;
      case 'q':
	run = FALSE;
	back = TRUE;
	break;
    }
    if (!back) {
      showWhere(getClass(machine.currentReceiver),
		machine.currentClass,
		machine.currentSelector);
    }
  }
}


/*------------------------------------*/
/* Auxiliary Functions                */
/*------------------------------------*/


void push(ObjPtr object)
{
  setPointer(machine.currentStack, machine.sp++, object);
}


ObjPtr pop(void)
{
  return getPointer(machine.currentStack, --machine.sp);
}


void storeContextRegisters(void)
{
  ObjPtr object;

  object = newSmallInteger(machine.ip);
  setPointer(machine.currentActiveContext, INSTPTR_IN_CONTEXT, object);
  object = newSmallInteger(machine.sp);
  setPointer(machine.currentActiveContext, STACKPTR_IN_CONTEXT, object);
}


void fetchContextRegisters(void)
{
  ObjPtr contextClass;

  contextClass = getClass(machine.currentActiveContext);
  if (contextClass ==
      getPointer(machine.BlockContext, VALUE_IN_ASSOCIATION)) {
    machine.currentHomeContext = getPointer(machine.currentActiveContext,
					    HOME_IN_BLOCKCONTEXT);
  } else
  if (contextClass ==
      getPointer(machine.MethodContext, VALUE_IN_ASSOCIATION)) {
    machine.currentHomeContext = machine.currentActiveContext;
  } else {
    error("illegal class of active context");
  }
  machine.currentSender =
    getPointer(machine.currentHomeContext, SENDER_IN_METHODCONTEXT);
  machine.currentCaller =
    getPointer(machine.currentActiveContext, CALLER_IN_BLOCKCONTEXT);
  machine.currentClass =
    getPointer(machine.currentHomeContext, CLASS_IN_METHODCONTEXT);
  machine.currentMethod =
    getPointer(machine.currentHomeContext, METHOD_IN_METHODCONTEXT);
  machine.currentSelector =
    getPointer(machine.currentMethod, SELECTOR_IN_COMPILEDMETHOD);
  machine.currentBytecodes =
    getPointer(machine.currentMethod, BYTECODES_IN_COMPILEDMETHOD);
  machine.currentLiterals =
    getPointer(machine.currentMethod, LITERALS_IN_COMPILEDMETHOD);
  machine.ip =
    smallIntegerValue(getPointer(machine.currentActiveContext,
				 INSTPTR_IN_CONTEXT));
  machine.currentReceiver =
    getPointer(machine.currentHomeContext, RECEIVER_IN_METHODCONTEXT);
  machine.currentTemporaries =
    getPointer(machine.currentHomeContext, TEMPORARIES_IN_METHODCONTEXT);
  machine.currentStack =
    getPointer(machine.currentActiveContext, STACK_IN_CONTEXT);
  machine.sp =
    smallIntegerValue(getPointer(machine.currentActiveContext,
				 STACKPTR_IN_CONTEXT));
}


/*------------------------------------*/
/* Bytecode Interpreter               */
/*------------------------------------*/


void findMethod(ObjPtr initialClass, ObjPtr selector)
{
  ObjPtr clazz;
  ObjPtr dictionary;
  ObjPtr associations;
  ObjPtr association;

  clazz = initialClass;
  while (clazz != machine.nil) {
    dictionary = getPointer(clazz, METHODDICTIONARY_IN_CLASS);
    associations = getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY);
    while (associations != machine.nil) {
      association = getPointer(associations, OBJECT_IN_LINKEDOBJECT);
      if (selector == getPointer(association, KEY_IN_ASSOCIATION)) {
	/* method found, set newClass and newMethod */
	machine.newClass = clazz;
	machine.newMethod = getPointer(association, VALUE_IN_ASSOCIATION);
	return;
      }
      associations = getPointer(associations, NEXTLINK_IN_LINKEDOBJECT);
    }
    clazz = getPointer(clazz, SUPERCLASS_IN_CLASS);
  }
  /* no method found */
  printString(getPointer(initialClass, NAME_IN_CLASS));
  printf(" >> ");
  printString(selector);
  error("message not understood");
}


void executeNewMethod(int numArgs)
{
  ObjPtr primitive;
  Ushort primitiveNumber;
  ObjPtr object;
  Ushort tempsize, i;
  Ushort stacksize;

  /* check if message can be primitively responded to */
  primitive = getPointer(machine.newMethod, PRIMITIVE_IN_COMPILEDMETHOD);
  if (primitive != machine.nil) {
    primitiveNumber = smallIntegerValue(primitive);
    if ((*primitiveMethods[primitiveNumber])(numArgs)) {
      /* primitive succeeded, do not further execute newMethod */
      if (debugInterpreter) {
	printf("<responded to by primitive %d>\n", primitiveNumber);
      }
      return;
    }
  }
  /* no primitive specified, or primitive failed: execute newMethod */
  /* construct new context */
  machine.newContext =
    allocateObject(getPointer(machine.MethodContext, VALUE_IN_ASSOCIATION),
		   SIZE_OF_METHODCONTEXT,
		   TRUE);
  setPointer(machine.newContext,
	     SENDER_IN_METHODCONTEXT,
	     machine.currentActiveContext);
  setPointer(machine.newContext,
	     CLASS_IN_METHODCONTEXT,
	     machine.newClass);
  setPointer(machine.newContext,
	     METHOD_IN_METHODCONTEXT,
	     machine.newMethod);
  object = newSmallInteger(0);
  setPointer(machine.newContext,
	     INSTPTR_IN_METHODCONTEXT,
	     object);
  tempsize = smallIntegerValue(getPointer(machine.newMethod,
					  TEMPSIZE_IN_COMPILEDMETHOD));
  object = allocateObject(getPointer(machine.Array, VALUE_IN_ASSOCIATION),
			  tempsize,
			  TRUE);
  setPointer(machine.newContext,
	     TEMPORARIES_IN_METHODCONTEXT,
	     object);
  for (i = 1; i <= numArgs; i++) {
    setPointer(object, numArgs - i, pop());
  }
  setPointer(machine.newContext,
	     RECEIVER_IN_METHODCONTEXT,
	     pop());
  stacksize = smallIntegerValue(getPointer(machine.newMethod,
					   STACKSIZE_IN_COMPILEDMETHOD));
  object = allocateObject(getPointer(machine.Array, VALUE_IN_ASSOCIATION),
			  stacksize,
			  TRUE);
  setPointer(machine.newContext,
	     STACK_IN_METHODCONTEXT,
	     object);
  object = newSmallInteger(0);
  setPointer(machine.newContext,
	     STACKPTR_IN_METHODCONTEXT,
	     object);
  /* make the new context the current context */
  storeContextRegisters();
  machine.currentActiveContext = machine.newContext;
  fetchContextRegisters();
}


void runInterpreter(void)
{
  Uchar opcode;
  Uchar value;
  ObjPtr returnedObject;
  Uchar selectorNumber;
  ObjPtr selector;
  ObjPtr clazz;
  Ushort offset;
  Ushort stacksize;
  ObjPtr object;

  run = TRUE;
  while (1) {
    /* test if debugging mode is on */
    if (debugInterpreter) {
      debug();
    }
    /* test run flag here because debug() may have switched it off */
    if (!run) {
      break;
    }
    /* now fetch and execute instruction */
    opcode = getByte(machine.currentBytecodes, machine.ip++);
    value = LO4(opcode);
    opcode = HI4(opcode);
    if (opcode == HI4(OP_EXTENDED)) {
      opcode = value;
      value = getByte(machine.currentBytecodes, machine.ip++);
    }
    switch (opcode) {
      case HI4(OP_SPECIALS):
	switch (value) {
	  case LO4(OP_NOP):
		break;
	  case LO4(OP_PUSHSELF):
		push(machine.currentReceiver);
		break;
	  case LO4(OP_PUSHNIL):
		push(machine.nil);
		break;
	  case LO4(OP_PUSHFALSE):
		push(machine.false);
		break;
	  case LO4(OP_PUSHTRUE):
		push(machine.true);
		break;
	  case LO4(OP_DUP):
		push(getPointer(machine.currentStack, machine.sp - 1));
		break;
	  case LO4(OP_POP):
		pop();
		break;
	  case LO4(OP_RET):
		/* check if we try to return to a context
		   that has already been returned from */
		if (machine.currentSender == machine.nil) {
		  error("cannot return");
		}
		/* set flag that we cannot return again */
		setPointer(machine.currentHomeContext,
			   SENDER_IN_METHODCONTEXT,
			   machine.nil);
		/* get object which should be returned */
		returnedObject = pop();
		/* change contexts */
		machine.currentActiveContext = machine.currentSender;
		fetchContextRegisters();
		/* push returned object on stack */
		push(returnedObject);
		if (debugInterpreter) {
		  showWhere(getClass(machine.currentReceiver),
			    machine.currentClass,
			    machine.currentSelector);
		}
		break;
	  case LO4(OP_RETBLOCK):
		/* get object which should be returned */
		returnedObject = pop();
		/* change contexts */
		machine.currentActiveContext = machine.currentCaller;
		fetchContextRegisters();
		/* push returned object on stack */
		push(returnedObject);
		if (debugInterpreter) {
		  showWhere(getClass(machine.currentReceiver),
			    machine.currentClass,
			    machine.currentSelector);
		}
		break;
	  default:
		error("illegal special bytecode encountered");
		break;
	}
	break;
      case HI4(OP_PUSHLTRL):
	push(getPointer(machine.currentLiterals, value));
	break;
      case HI4(OP_PUSHINST):
	push(getPointer(machine.currentReceiver, value));
	break;
      case HI4(OP_STOREINST):
	setPointer(machine.currentReceiver, value, pop());
	break;
      case HI4(OP_PUSHTEMP):
	push(getPointer(machine.currentTemporaries, value));
	break;
      case HI4(OP_STORETEMP):
	setPointer(machine.currentTemporaries, value, pop());
	break;
      case HI4(OP_PUSHASSOC):
	push(getPointer(getPointer(machine.currentLiterals, value),
			VALUE_IN_ASSOCIATION));
	break;
      case HI4(OP_STOREASSOC):
	setPointer(getPointer(machine.currentLiterals, value),
		   VALUE_IN_ASSOCIATION, pop());
	break;
      case HI4(OP_SEND):
      case HI4(OP_SENDSUPER):
	/* get the selector of the message */
	selectorNumber = getByte(machine.currentBytecodes, machine.ip++);
	selector = getPointer(machine.currentLiterals, selectorNumber);
	/* determine the class where to start the search */
	if (opcode != HI4(OP_SENDSUPER)) {
	  /* SEND starts in receiver's class */
	  clazz = getClass(getPointer(machine.currentStack,
				      machine.sp - value - 1));
	} else {
	  /* SENDSUPER starts in superclass of method's class */
	  clazz = getPointer(machine.currentClass, SUPERCLASS_IN_CLASS);
	}
	/* findMethod() sets machine.newClass and machine.newMethod */
	findMethod(clazz, selector);
	if (debugInterpreter) {
	  showWhere(clazz, machine.newClass, selector);
	}
	/* check number of arguments */
	if (value !=
	    smallIntegerValue(getPointer(machine.newMethod,
					 NUMBERARGS_IN_COMPILEDMETHOD))) {
	  error("wrong number of arguments in message send");
	}
	executeNewMethod(value);
	break;
      case HI4(OP_JUMPS):
	offset = getByte(machine.currentBytecodes, machine.ip++);
	offset <<= 8;
	offset |= getByte(machine.currentBytecodes, machine.ip++);
	switch (value) {
	  case LO4(OP_JUMP):
		machine.ip = offset;
		break;
	  default:
		error("illegal jump bytecode encountered");
		break;
	}
	break;
      case HI4(OP_PUSHBLOCK):
	stacksize = getByte(machine.currentBytecodes, machine.ip++);
	machine.newContext =
	  allocateObject(getPointer(machine.BlockContext,
				    VALUE_IN_ASSOCIATION),
			 SIZE_OF_BLOCKCONTEXT,
			 TRUE);
	object = allocateObject(getPointer(machine.Array,
					   VALUE_IN_ASSOCIATION),
				stacksize,
				TRUE);
	setPointer(machine.newContext,
		   STACK_IN_BLOCKCONTEXT,
		   object);
	object = newSmallInteger(value);
	setPointer(machine.newContext,
		   NUMBERARGS_IN_BLOCKCONTEXT,
		   object);
	/* the block's initial ip is 3 more than the current ip
	   because a JUMP always follows the PUSHBLOCK instruction */
	object = newSmallInteger(machine.ip + 3);
	setPointer(machine.newContext,
		   INITIALIP_IN_BLOCKCONTEXT,
		   object);
	setPointer(machine.newContext,
		   HOME_IN_BLOCKCONTEXT,
		   machine.currentHomeContext);
	push(machine.newContext);
	break;
      case HI4(OP_UNUSED_Cx):
	error("illegal bytecode UNUSED_Cx encountered");
	break;
      case HI4(OP_UNUSED_Dx):
	error("illegal bytecode UNUSED_Dx encountered");
	break;
      case HI4(OP_UNUSED_Ex):
	error("illegal bytecode UNUSED_Ex encountered");
	break;
      case HI4(OP_EXTENDED):
	error("illegal bytecode EXTENDED encountered");
	break;
    }
  }
}
