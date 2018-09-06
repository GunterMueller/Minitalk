/*
 * tree.c -- abstract syntax tree
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "utils.h"
#include "machine.h"
#include "struct.h"
#include "memory.h"
#include "tree.h"


/*------------------------------------*/
/* String Handling                    */
/*------------------------------------*/


char *copyString(char *string) {
  char *newString;

  newString = allocate(strlen(string) + 1);
  strcpy(newString, string);
  return newString;
}


char *appendString(char *alreadyThere, char *toBeAppended) {
  char *result;

  if (alreadyThere == NULL) {
    return copyString(toBeAppended);
  }
  result = allocate(strlen(alreadyThere) + strlen(toBeAppended) + 1);
  strcpy(result, alreadyThere);
  release(alreadyThere);
  strcat(result, toBeAppended);
  return result;
}


/*------------------------------------*/
/* Variable Table                     */
/*------------------------------------*/


Bool isSuper(Node *expression) {
  return expression->type == N_VARIABLE &&
	 expression->u.variable.record->type == V_SUPER;
}


Bool isAssignable(Variable *variable) {
  return variable->type == V_INSTANCE ||
	 variable->type == V_TEMPORARY ||
	 variable->type == V_SHARED;
}


Variable *lookupVariable(char *name, Variable **variables) {
  Variable *variable;

  variable = *variables;
  while (variable != NULL) {
    if (strcmp(variable->name, name) == 0) {
      return variable;
    }
    variable = variable->next;
  }
  return NULL;
}


Variable *enterVariable(int type, char *name, Variable **variables) {
  Variable *variable;
  Variable *scan;

  if (lookupVariable(name, variables) != NULL) {
    return NULL;
  }
  variable = allocate(sizeof(Variable));
  variable->next = NULL;
  variable->type = type;
  variable->name = copyString(name);
  if (*variables == NULL) {
    *variables = variable;
    return variable;
  }
  scan = *variables;
  while (scan->next != NULL) {
    scan = scan->next;
  }
  scan->next = variable;
  return variable;
}


#define MAX_NAME_SIZE		LINE_SIZE


void initVariables(Variable **variables, ObjPtr aClass) {
  ObjPtr lastClass, class;
  ObjPtr instVarArray, instVar;
  int numberInstVars, i;
  int nameSize;
  char name[MAX_NAME_SIZE];
  ObjPtr dictionary, associations;
  ObjPtr association, key;

  /* init the variable list pointer */
  *variables = NULL;
  /* enter pseudo variables */
  enterVariable(V_SELF,  "self",  variables);
  enterVariable(V_SUPER, "super", variables);
  enterVariable(V_NIL,   "nil",   variables);
  enterVariable(V_FALSE, "false", variables);
  enterVariable(V_TRUE,  "true",  variables);
  /* enter instance variables of superclass chain, begin with root class */
  lastClass = machine.nil;
  while (lastClass != aClass) {
    class = aClass;
    /* skip classes until the class before lastClass is reached */
    while (getPointer(class, SUPERCLASS_IN_CLASS) != lastClass) {
      class = getPointer(class, SUPERCLASS_IN_CLASS);
    }
    /* now add the instance variables of this class */
    instVarArray = getPointer(class, INSTVARS_IN_CLASS);
    numberInstVars = getSize(instVarArray);
    for (i = 0; i < numberInstVars; i++) {
      instVar = getPointer(instVarArray, i);
      nameSize = getSize(instVar);
      if (nameSize >= MAX_NAME_SIZE) {
	error("instance variable name too long");
      }
      strncpy(name, getBytes(instVar), nameSize);
      name[nameSize] = '\0';
      if (enterVariable(V_INSTANCE, name, variables) == NULL) {
        error("variable '%s' is already defined", name);
      }
    }
    /* work towards aClass */
    lastClass = class;
  }
  /* enter global variables */
  dictionary = getPointer(machine.MiniTalk, VALUE_IN_ASSOCIATION);
  associations = getPointer(dictionary, ASSOCIATIONS_IN_DICTIONARY);
  while (associations != machine.nil) {
    association = getPointer(associations, OBJECT_IN_LINKEDOBJECT);
    key = getPointer(association, KEY_IN_ASSOCIATION);
    nameSize = getSize(key);
    if (nameSize >= MAX_NAME_SIZE) {
      error("global variable name too long");
    }
    strncpy(name, getBytes(key), nameSize);
    name[nameSize] = '\0';
    if (enterVariable(V_SHARED, name, variables) == NULL) {
      error("variable '%s' is already defined", name);
    }
    associations = getPointer(associations, NEXTLINK_IN_LINKEDOBJECT);
  }
}


void computeOffsets(Variable **variables, Node *method) {
  int numberInstances;
  int numberArguments;
  int numberTemporaries;
  Variable *variable;

  numberInstances = 0;
  numberArguments = 0;
  numberTemporaries = 0;
  variable = *variables;
  while (variable != NULL) {
    switch (variable->type) {
      case V_SELF:
      case V_SUPER:
      case V_NIL:
      case V_FALSE:
      case V_TRUE:
	variable->offset = 0;
	break;
      case V_INSTANCE:
	variable->offset = numberInstances++;
	break;
      case V_ARGUMENT:
	variable->offset = numberArguments++;
	break;
      case V_TEMPORARY:
	variable->offset = numberTemporaries++;
	break;
      case V_SHARED:
	variable->offset = 0;
	break;
      default:
	error("computeOffsets has illegal variable type");
	break;
    }
    variable = variable->next;
  }
  variable = *variables;
  while (variable != NULL) {
    if (variable->type == V_TEMPORARY) {
      variable->offset += numberArguments;
    }
    variable = variable->next;
  }
  method->u.method.numberArguments = numberArguments;
  method->u.method.numberTemporaries = numberArguments + numberTemporaries;
}


static void showVariable(Variable *record) {
  switch (record->type) {
    case V_SELF:
    case V_SUPER:
    case V_NIL:
    case V_FALSE:
    case V_TRUE:
	break;
    case V_INSTANCE:
	printf("I%02d:", record->offset);
	break;
    case V_ARGUMENT:
	printf("A%02d:", record->offset);
	break;
    case V_TEMPORARY:
	printf("T%02d:", record->offset);
	break;
    case V_SHARED:
	printf("S:");
	break;
    default:
	error("showVariable has illegal variable type");
	break;
  }
  printf("%s", record->name);
}


void showVariables(Variable **variables) {
  Variable *variable;

  variable = *variables;
  printf("VARIABLES = {");
  if (variable != NULL) {
    while (variable->next != NULL) {
      showVariable(variable);
      printf(", ");
      variable = variable->next;
    }
    showVariable(variable);
  }
  printf("}\n");
}


void freeVariables(Variable **variables) {
  Variable *variable;
  Variable *next;

  variable = *variables;
  while (variable != NULL) {
    release(variable->name);
    next = variable->next;
    release(variable);
    variable = next;
  }
  *variables = NULL;
}


/*------------------------------------*/
/* Parse Tree Nodes                   */
/*------------------------------------*/


Node *newNode(int type) {
  Node *np;

  np = allocate(sizeof(Node));
  np->type = type;
  return np;
}


List *appendElement(List *list, Node *element) {
  List *newElement;
  List *scan;

  newElement = allocate(sizeof(List));
  newElement->head = element;
  newElement->tail = NULL;
  if (list == NULL) {
    return newElement;
  }
  scan = list;
  while (scan->tail != NULL) {
    scan = scan->tail;
  }
  scan->tail = newElement;
  return list;
}


static void showList(List *list, Bool newline, char *separator) {
  printf("{");
  if (list != NULL) {
    if (newline) {
      printf("\n");
    }
    while (list->tail != NULL) {
      showTree(list->head);
      printf(separator);
      if (newline) {
	printf("\n");
      }
      list = list->tail;
    }
    showTree(list->head);
    if (newline) {
      printf("\n");
    }
  }
  printf("}");
}


static void freeList(List *list) {
  List *next;

  while (list != NULL) {
    freeTree(list->head);
    next = list->tail;
    release(list);
    list = next;
  }
}


void showTree(Node *tree) {
  if (tree == NULL) {
    error("showTree has empty tree");
  }
  switch (tree->type) {
    case N_SYMBOL:
	printf("#%s", tree->u.symbol.name);
	break;
    case N_INTNUM:
	printf("%ld", tree->u.intnum.value);
	break;
    case N_FLONUM:
	printf("%e", tree->u.flonum.value);
	break;
    case N_STRING:
	printf("'%s'", tree->u.string.value);
	break;
    case N_CHARCON:
	printf("$%c", tree->u.charcon.value);
	break;
    case N_ARRAY:
	printf("#(");
	showList(tree->u.array.elements, false, ", ");
	printf(")");
	break;
    case N_VARIABLE:
	showVariable(tree->u.variable.record);
	break;
    case N_BLOCK:
	printf("[");
	showList(tree->u.block.variables, false, ", ");
	printf(" | ");
	showList(tree->u.block.statements, false, ". ");
	printf("]");
	break;
    case N_MESSAGE:
	printf("(");
	if (tree->u.message.receiver != NULL) {
	  /* non-cascaded messages have a non-null receiver */
	  showTree(tree->u.message.receiver);
	} else {
	  /* cascaded messages have a null receiver */
	  printf(";");
	}
	printf(") ");
	if (tree->u.message.superFlag) {
	  printf("*");
	}
	showTree(tree->u.message.selector);
	printf(" ");
	showList(tree->u.message.arguments, false, ", ");
	break;
    case N_CASCADE:
	printf("(");
	showTree(tree->u.cascade.receiver);
	printf(") ");
	showList(tree->u.cascade.messages, false, "; ");
	break;
    case N_ASSIGN:
	showList(tree->u.assign.variables, false, ", ");
	printf(" <- ");
	showTree(tree->u.assign.expression);
	break;
    case N_RETEXP:
	printf("^ ");
	showTree(tree->u.retexp.expression);
	break;
    case N_METHOD:
	printf("METHOD selector    = ");
	showTree(tree->u.method.selector);
	printf("\n");
	printf("       numberArgs  = ");
	printf("%d", tree->u.method.numberArguments);
	printf("\n");
	printf("       numberTemps = ");
	printf("%d", tree->u.method.numberTemporaries);
	printf("\n");
	printf("       primitive   = ");
	printf("%d", tree->u.method.primitive);
	printf("\n");
	printf("       statements  = ");
	showList(tree->u.method.statements, true, ".");
	printf("\n");
	break;
    default:
	error("showTree has unknown node type");
	break;
  }
}


void freeTree(Node *tree) {
  if (tree == NULL) {
    error("freeTree has empty tree");
  }
  switch (tree->type) {
    case N_SYMBOL:
	release(tree->u.symbol.name);
	break;
    case N_INTNUM:
	break;
    case N_FLONUM:
	break;
    case N_STRING:
	release(tree->u.string.value);
	break;
    case N_CHARCON:
	break;
    case N_ARRAY:
	freeList(tree->u.array.elements);
	break;
    case N_VARIABLE:
	/* variable table records are freed separately */
	break;
    case N_BLOCK:
	freeList(tree->u.block.variables);
	freeList(tree->u.block.statements);
	break;
    case N_MESSAGE:
	if (tree->u.message.receiver != NULL) {
	  /* cascaded messages have a null receiver */
	  freeTree(tree->u.message.receiver);
	}
	freeTree(tree->u.message.selector);
	freeList(tree->u.message.arguments);
	break;
    case N_CASCADE:
	freeTree(tree->u.cascade.receiver);
	freeList(tree->u.cascade.messages);
	break;
    case N_ASSIGN:
	freeList(tree->u.assign.variables);
	freeTree(tree->u.assign.expression);
	break;
    case N_RETEXP:
	freeTree(tree->u.retexp.expression);
	break;
    case N_METHOD:
	freeTree(tree->u.method.selector);
	freeList(tree->u.method.statements);
	break;
    default:
	error("freeTree has unknown node type");
	break;
  }
  release(tree);
}
