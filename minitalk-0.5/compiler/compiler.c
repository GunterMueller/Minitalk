/*
 * compiler.c -- MiniTalk-to-bytecode compiler
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
#include "opcodes.h"


Bool debugTree = false;


/*------------------------------------*/
/* Bytecode Generator                 */
/*------------------------------------*/


static int currentStacksize;
static int maxStacksize;


static void updateStack(int stackChange) {
  currentStacksize += stackChange;
  if (currentStacksize < 0) {
    error("coder tried to code a pop from an empty stack");
  }
  if (currentStacksize > maxStacksize) {
    maxStacksize = currentStacksize;
  }
}


#define MAX_LITERAL_SIZE	256

static Node *literalArray[MAX_LITERAL_SIZE];
static Word literalSize;


static Word makeLiteral(Node *literal) {
  if (literalSize == MAX_LITERAL_SIZE) {
    error("too many literals in method");
  }
  literalArray[literalSize] = literal;
  return literalSize++;
}


#define MAX_CODE_SIZE		15000

static Byte codeArray[MAX_CODE_SIZE];
static Word codeSize;


static void codeByte(Byte byte) {
  if (codeSize == MAX_CODE_SIZE) {
    error("too many bytecodes in method");
  }
  codeArray[codeSize++] = byte;
}


static Word getCurrentOffset(void) {
  return codeSize;
}


static void codeOffset(Word offset) {
  codeByte(offset >> 8);
  codeByte(offset & 0xFF);
}


static void patchOffset(Word address, Word offset) {
  codeArray[address + 0] = offset >> 8;
  codeArray[address + 1] = offset & 0xFF;
}


static void code0(Byte bytecode, int stackChange) {
  codeByte(bytecode);
  updateStack(stackChange);
}


static void code1(Byte bytecode, Byte value, int stackChange) {
  if (value < 16) {
    codeByte(bytecode | value);
  } else {
    codeByte(OP_EXTENDED | (bytecode >> 4));
    codeByte(value);
  }
  updateStack(stackChange);
}


static void code2(Byte bytecode, Byte value, Byte param, int stackChange) {
  if (value < 16) {
    codeByte(bytecode | value);
  } else {
    codeByte(OP_EXTENDED | (bytecode >> 4));
    codeByte(value);
  }
  codeByte(param);
  updateStack(stackChange);
}


static void code3(Byte bytecode, Word offset, int stackChange) {
  codeByte(bytecode);
  codeOffset(offset);
  updateStack(stackChange);
}


static void codeLoad(Node *expression) {
  Variable *variable;

  variable = expression->u.variable.record;
  switch (variable->type) {
    case V_SELF:
	code0(OP_PUSHSELF, 1);
	break;
    case V_SUPER:
	code0(OP_PUSHSELF, 1);
	break;
    case V_NIL:
	code0(OP_PUSHNIL, 1);
	break;
    case V_FALSE:
	code0(OP_PUSHFALSE, 1);
	break;
    case V_TRUE:
	code0(OP_PUSHTRUE, 1);
	break;
    case V_INSTANCE:
	code1(OP_PUSHINST, variable->offset, 1);
	break;
    case V_ARGUMENT:
	code1(OP_PUSHTEMP, variable->offset, 1);
	break;
    case V_TEMPORARY:
	code1(OP_PUSHTEMP, variable->offset, 1);
	break;
    case V_SHARED:
	code1(OP_PUSHASSOC, makeLiteral(expression), 1);
	break;
    default:
	error("codeLoad has illegal variable type");
	break;
  }
}


static void codeStore(Node *expression) {
  Variable *variable;

  variable = expression->u.variable.record;
  switch (variable->type) {
    case V_INSTANCE:
	code1(OP_STOREINST, variable->offset, -1);
	break;
    case V_TEMPORARY:
	code1(OP_STORETEMP, variable->offset, -1);
	break;
    case V_SHARED:
	code1(OP_STOREASSOC, makeLiteral(expression), -1);
	break;
    default:
	error("codeStore has illegal variable type");
	break;
  }
}


static void codeExpression(Node *expression, Bool valueNeeded) {
  int numberArguments;
  List *arguments;
  List *messages;
  List *variables;
  Word patchLocation;
  List *statements;

  if (expression == NULL) {
    error("coder has empty expression");
  }
  switch (expression->type) {
    case N_SYMBOL:
    case N_INTNUM:
    case N_FLONUM:
    case N_STRING:
    case N_CHARCON:
    case N_ARRAY:
	if (valueNeeded) {
	  code1(OP_PUSHLTRL, makeLiteral(expression), 1);
	}
	break;
    case N_VARIABLE:
	if (valueNeeded) {
	  codeLoad(expression);
	}
	break;
    case N_BLOCK:
	if (valueNeeded) {
	  code2(OP_PUSHBLOCK, expression->u.block.numberVariables, 10, 1);
	  code3(OP_JUMP, 0, 0);
	  patchLocation = getCurrentOffset() - 2;
	  updateStack(expression->u.block.numberVariables);
	  variables = expression->u.block.variables;
	  while (variables != NULL) {
	    codeStore(variables->head);
	    variables = variables->tail;
	  }
	  statements = expression->u.block.statements;
	  if (statements == NULL) {
	    code0(OP_PUSHNIL, 1);
	    code0(OP_RETBLOCK, -1);
	  } else {
	    while (statements->tail != NULL) {
	      expression = statements->head;
	      codeExpression(expression, false);
	      statements = statements->tail;
	    }
	    expression = statements->head;
	    if (expression->type == N_RETEXP) {
	      codeExpression(expression->u.retexp.expression, true);
	      code0(OP_RET, -1);
	    } else {
	      codeExpression(expression, true);
	      code0(OP_RETBLOCK, -1);
	    }
	  }
	  patchOffset(patchLocation, getCurrentOffset());
	}
	break;
    case N_MESSAGE:
	/* ATTENTION: if we have a null receiver (a cascaded message) */
	/* then do nothing since the receiver has already been pushed */
	if (expression->u.message.receiver != NULL) {
	  codeExpression(expression->u.message.receiver, true);
	}
	numberArguments = 0;
	arguments = expression->u.message.arguments;
	while (arguments != NULL) {
	  codeExpression(arguments->head, true);
	  numberArguments++;
	  arguments = arguments->tail;
	}
	if (!expression->u.message.superFlag) {
	  code2(OP_SEND,
		numberArguments,
		makeLiteral(expression->u.message.selector),
		-numberArguments);
	} else {
	  code2(OP_SENDSUPER,
		numberArguments,
		makeLiteral(expression->u.message.selector),
		-numberArguments);
	}
	if (!valueNeeded) {
	  code0(OP_POP, -1);
	}
	break;
    case N_CASCADE:
	codeExpression(expression->u.cascade.receiver, true);
	messages = expression->u.cascade.messages;
	while (messages->tail != NULL) {
	  code0(OP_DUP, 1);
	  codeExpression(messages->head, false);
	  messages = messages->tail;
	}
	codeExpression(messages->head, valueNeeded);
	break;
    case N_ASSIGN:
	codeExpression(expression->u.assign.expression, true);
	variables = expression->u.assign.variables;
	while (variables->tail != NULL) {
	  code0(OP_DUP, 1);
	  codeStore(variables->head);
	  variables = variables->tail;
	}
	if (valueNeeded) {
	  code0(OP_DUP, 1);
	}
	codeStore(variables->head);
	break;
    case N_METHOD:
	/* If valueNeeded is true, then return the value of the last
	   expression (instead of returning self). This is needed in
	   case of interactively evaluating an expression. */
	statements = expression->u.method.statements;
	if (statements == NULL) {
	  code0(OP_PUSHSELF, 1);
	  code0(OP_RET, -1);
	} else {
	  while (statements->tail != NULL) {
	    expression = statements->head;
	    codeExpression(expression, false);
	    statements = statements->tail;
	  }
	  expression = statements->head;
	  if (expression->type == N_RETEXP) {
	    codeExpression(expression->u.retexp.expression, true);
	    code0(OP_RET, -1);
	  } else {
	    if (valueNeeded) {
	      codeExpression(expression, true);
	      code0(OP_RET, -1);
	    } else {
	      codeExpression(expression, false);
	      code0(OP_PUSHSELF, 1);
	      code0(OP_RET, -1);
	    }
	  }
	}
	break;
    default:
	error("coder has illegal tree node");
	break;
  }
}


static ObjPtr lookupGlobal(char *name) {
  int length;
  ObjPtr dictionary;
  ObjPtr associations;
  ObjPtr association;
  ObjPtr key;

  length = strlen(name);
  dictionary = getPointer(machine.MiniTalk, VALUE_IN_ASSOCIATION);
  associations = getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY);
  while (associations != machine.nil) {
    association = getPointer(associations, OBJECT_IN_LINKEDOBJECT);
    key = getPointer(association, KEY_IN_ASSOCIATION);
    if (strncmp(getBytes(key), name, length) == 0) {
      return association;
    }
    associations = getPointer(associations, NEXTLINK_IN_LINKEDOBJECT);
  }
  return machine.nil;
}


static void codeLiteral(Node *literalNode) {
  ObjPtr object;
  List *elements;
  int i;
  char *name;

  switch (literalNode->type) {
    case N_SYMBOL:
	machine.compilerLiteral =
	  newSymbol(literalNode->u.symbol.name);
	break;
    case N_INTNUM:
	machine.compilerLiteral =
	  newSmallInteger(literalNode->u.intnum.value);
	break;
    case N_FLONUM:
	machine.compilerLiteral =
	  newFloat(literalNode->u.flonum.value);
	break;
    case N_STRING:
	machine.compilerLiteral =
	  newString(literalNode->u.string.value);
	break;
    case N_CHARCON:
	machine.compilerLiteral =
	  newCharacter(literalNode->u.charcon.value);
	break;
    case N_ARRAY:
	/* Arrays must be constructed recursively. Use  */
	/* machine.compilerLiterals as temporary stack. */
	object =
	  allocateObject(getPointer(machine.LinkedObject,
				    VALUE_IN_ASSOCIATION),
			 SIZE_OF_LINKEDOBJECT,
			 true);
	setPointer(object,
		   NEXTLINK_IN_LINKEDOBJECT,
		   machine.compilerLiterals);
	machine.compilerLiterals = object;
	object =
	  allocateObject(getPointer(machine.Array,
				    VALUE_IN_ASSOCIATION),
			 literalNode->u.array.numberElements,
			 true);
	setPointer(machine.compilerLiterals,
		   OBJECT_IN_LINKEDOBJECT,
		   object);
	elements = literalNode->u.array.elements;
	i = 0;
	while (elements != NULL) {
	  codeLiteral(elements->head);
	  setPointer(getPointer(machine.compilerLiterals,
				OBJECT_IN_LINKEDOBJECT),
		     i,
		     machine.compilerLiteral);
	  elements = elements->tail;
	  i++;
	}
	machine.compilerLiteral =
	  getPointer(machine.compilerLiterals, OBJECT_IN_LINKEDOBJECT);
	machine.compilerLiterals =
	  getPointer(machine.compilerLiterals, NEXTLINK_IN_LINKEDOBJECT);
	break;
    case N_VARIABLE:
	/* only shared variables appear here */
	name = literalNode->u.variable.record->name;
	machine.compilerLiteral = lookupGlobal(name);
	break;
    default:
	error("literal generator has illegal node type");
	break;
  }
}


static void codeMethod(Node *method, Bool lastValueNeeded) {
  int i;
  ObjPtr selector;

  maxStacksize = 0;
  currentStacksize = 0;
  codeSize = 0;
  literalSize = 0;
  codeExpression(method, lastValueNeeded);
  if (codeSize != 0) {
    machine.compilerCode =
      allocateObject(getPointer(machine.ByteArray, VALUE_IN_ASSOCIATION),
		     codeSize,
		     false);
    memcpy(getBytes(machine.compilerCode), codeArray, codeSize);
  } else {
    machine.compilerCode = machine.nil;
  }
  if (literalSize != 0) {
    machine.compilerLiterals =
      allocateObject(getPointer(machine.Array, VALUE_IN_ASSOCIATION),
		     literalSize,
		     true);
    for (i = 0; i < literalSize; i++) {
      codeLiteral(literalArray[i]);
      setPointer(machine.compilerLiterals,
		 i,
		 machine.compilerLiteral);
    }
  } else {
    machine.compilerLiterals = machine.nil;
  }
  machine.compilerMethod =
    allocateObject(getPointer(machine.CompiledMethod, VALUE_IN_ASSOCIATION),
		   SIZE_OF_COMPILEDMETHOD,
		   true);
  selector = newSymbol(method->u.method.selector->u.symbol.name);
  setPointer(machine.compilerMethod,
	     SELECTOR_IN_COMPILEDMETHOD,
	     selector);
  if (method->u.method.primitive != -1) {
    /* primitive call present */
    setPointer(machine.compilerMethod,
	       PRIMITIVE_IN_COMPILEDMETHOD,
	       newSmallInteger(method->u.method.primitive));
  }
  setPointer(machine.compilerMethod,
	     NUMBERARGS_IN_COMPILEDMETHOD,
	     newSmallInteger(method->u.method.numberArguments));
  setPointer(machine.compilerMethod,
	     TEMPSIZE_IN_COMPILEDMETHOD,
	     newSmallInteger(method->u.method.numberTemporaries));
  setPointer(machine.compilerMethod,
	     STACKSIZE_IN_COMPILEDMETHOD,
	     newSmallInteger(maxStacksize));
  setPointer(machine.compilerMethod,
	     BYTECODES_IN_COMPILEDMETHOD,
	     machine.compilerCode);
  setPointer(machine.compilerMethod,
	     LITERALS_IN_COMPILEDMETHOD,
	     machine.compilerLiterals);
  /* create association */
  machine.compilerAssociation =
    allocateObject(getPointer(machine.Association, VALUE_IN_ASSOCIATION),
		   SIZE_OF_ASSOCIATION,
		   true);
  selector = newSymbol(method->u.method.selector->u.symbol.name);
  setPointer(machine.compilerAssociation,
	     KEY_IN_ASSOCIATION,
	     selector);
  setPointer(machine.compilerAssociation,
	     VALUE_IN_ASSOCIATION,
	     machine.compilerMethod);
}


/*------------------------------------*/
/* Compiler Interface                 */
/*------------------------------------*/


void compile(char *aString, ObjPtr aClass, Bool valueNeeded) {
  Variable *variables;
  Node *method;

  initVariables(&variables, aClass);
  initScanner(aString);
  nextToken();
  method = parseMethod(&variables);
  if (tokenType != T_END) {
    parseError("additional characters after end of method");
  }
  computeOffsets(&variables, method);
  if (debugTree) {
    showVariables(&variables);
    showTree(method);
  }
  machine.compilerClass = aClass;
  codeMethod(method, valueNeeded);
  freeTree(method);
  freeVariables(&variables);
}
