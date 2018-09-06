
/*
 * TREE.C	parse tree
 */


#include "minitalk.h"


Boolean debugTree = FALSE;


/*------------------------------------*/
/* String Handling                    */
/*------------------------------------*/


char *copyString(char *string)
{
  char *newString;

  newString = (char *) malloc(strlen(string) + 1);
  if (newString == NULL) {
    error("cannot allocate string");
  }
  strcpy(newString, string);
  return newString;
}


char *appendString(char *alreadyThere, char *toBeAppended)
{
  char *result;

  if (alreadyThere == NULL) {
    return copyString(toBeAppended);
  }
  result = (char *) malloc(strlen(alreadyThere) + strlen(toBeAppended) + 1);
  if (result == NULL) {
    error("cannot allocate string");
  }
  strcpy(result, alreadyThere);
  free(alreadyThere);
  strcat(result, toBeAppended);
  return result;
}


/*------------------------------------*/
/* Variable Table                     */
/*------------------------------------*/


Boolean isAssignable(Variable *variable)
{
  return variable->type == V_INSTANCE ||
	 variable->type == V_TEMPORARY ||
	 variable->type == V_SHARED;
}


Variable *lookupVariable(char *name, Variable **variables)
{
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


Variable *enterVariable(int type, char *name, Variable **variables)
{
  Variable *variable;
  Variable *scan;

  if (lookupVariable(name, variables) != NULL) {
    parseError("variable is already defined");
  }
  variable = (Variable *) malloc(sizeof(Variable));
  if (variable == NULL) {
    error("cannot allocate variable record");
  }
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


#define MAX_NAME_SIZE			40


void initVariables(Variable **variables, ObjPtr aClass)
{
  ObjPtr lastClass, clazz;
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
    clazz = aClass;
    /* skip classes until the class before lastClass is reached */
    while (getPointer(clazz, SUPERCLASS_IN_CLASS) != lastClass) {
      clazz = getPointer(clazz, SUPERCLASS_IN_CLASS);
    }
    /* now add the instance variables of this class */
    instVarArray = getPointer(clazz, INSTVARS_IN_CLASS);
    numberInstVars = getSize(instVarArray);
    for (i = 0; i < numberInstVars; i++) {
      instVar = getPointer(instVarArray, i);
      nameSize = getSize(instVar);
      if (nameSize >= MAX_NAME_SIZE) {
	error("instance variable name too long");
      }
      strncpy(name, getBytes(instVar), nameSize);
      name[nameSize] = '\0';
      enterVariable(V_INSTANCE, name, variables);
    }
    /* work towards aClass */
    lastClass = clazz;
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
    enterVariable(V_SHARED, name, variables);
    associations = getPointer(associations, NEXTLINK_IN_LINKEDOBJECT);
  }
}


void computeOffsets(Variable **variables, Node *method)
{
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
  method->all.method.numberArguments = numberArguments;
  method->all.method.numberTemporaries = numberArguments + numberTemporaries;
}


void showVariable(Variable *record)
{
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


void showVariables(Variable **variables)
{
  Variable *variable;

  if (!debugTree) {
    return;
  }
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


void freeVariables(Variable **variables)
{
  Variable *variable;
  Variable *next;

  variable = *variables;
  while (variable != NULL) {
    free(variable->name);
    next = variable->next;
    free(variable);
    variable = next;
  }
  *variables = NULL;
}


/*------------------------------------*/
/* Parse Tree Nodes                   */
/*------------------------------------*/


Node *newNode(int type)
{
  Node *np;

  np = (Node *) malloc(sizeof(Node));
  if (np == NULL) {
    error("cannot allocate parse tree node");
  }
  np->type = type;
  return np;
}


Element *appendElement(Element *list, Node *element)
{
  Element *newElement;
  Element *scan;

  newElement = (Element *) malloc(sizeof(Element));
  if (newElement == NULL) {
    error("cannot allocate list element");
  }
  newElement->next = NULL;
  newElement->element = element;
  if (list == NULL) {
    return newElement;
  }
  scan = list;
  while (scan->next != NULL) {
    scan = scan->next;
  }
  scan->next = newElement;
  return list;
}


void showList(Element *list, Boolean newline, char *separator)
{
  printf("{");
  if (list != NULL) {
    if (newline) {
      printf("\n");
    }
    while (list->next != NULL) {
      showTree(list->element);
      printf(separator);
      if (newline) {
	printf("\n");
      }
      list = list->next;
    }
    showTree(list->element);
    if (newline) {
      printf("\n");
    }
  }
  printf("}");
}


void freeList(Element *list)
{
  Element *next;

  while (list != NULL) {
    freeTree(list->element);
    next = list->next;
    free(list);
    list = next;
  }
}


void showTree(Node *tree)
{
  if (!debugTree) {
    return;
  }
  if (tree == NULL) {
    error("showTree has empty tree");
  }
  switch (tree->type) {
    case N_SYMBOL:
	printf("#%s", tree->all.symbol.name);
	break;
    case N_INTNUM:
	printf("%ld", tree->all.intnum.value);
	break;
    case N_FLONUM:
	printf("%e", tree->all.flonum.value);
	break;
    case N_STRING:
	printf("'%s'", tree->all.string.value);
	break;
    case N_CHARCON:
	printf("$%c", tree->all.charcon.value);
	break;
    case N_ARRAY:
	printf("#(");
	showList(tree->all.array.elements, FALSE, ", ");
	printf(")");
	break;
    case N_VARIABLE:
	showVariable(tree->all.variable.record);
	break;
    case N_BLOCK:
	printf("[");
	showList(tree->all.block.variables, FALSE, ", ");
	printf(" | ");
	showList(tree->all.block.statements, FALSE, ". ");
	printf("]");
	break;
    case N_MESSAGE:
	printf("(");
	if (tree->all.message.receiver != NULL) {
	  /* non-cascaded messages have a non-null receiver */
	  showTree(tree->all.message.receiver);
	} else {
	  /* cascaded messages have a null receiver */
	  printf(";");
	}
	printf(") ");
	if (tree->all.message.superFlag) {
	  printf("*");
	}
	showTree(tree->all.message.selector);
	printf(" ");
	showList(tree->all.message.arguments, FALSE, ", ");
	break;
    case N_CASCADE:
	printf("(");
	showTree(tree->all.cascade.receiver);
	printf(") ");
	showList(tree->all.cascade.messages, FALSE, "; ");
	break;
    case N_ASSIGN:
	showList(tree->all.assign.variables, FALSE, ", ");
	printf(" <- ");
	showTree(tree->all.assign.expression);
	break;
    case N_RETEXP:
	printf("^ ");
	showTree(tree->all.retexp.expression);
	break;
    case N_METHOD:
	printf("METHOD selector    = ");
	showTree(tree->all.method.selector);
	printf("\n");
	printf("       numberArgs  = ");
	printf("%d", tree->all.method.numberArguments);
	printf("\n");
	printf("       numberTemps = ");
	printf("%d", tree->all.method.numberTemporaries);
	printf("\n");
	printf("       primitive   = ");
	printf("%d", tree->all.method.primitive);
	printf("\n");
	printf("       statements  = ");
	showList(tree->all.method.statements, TRUE, ".");
	printf("\n");
	break;
    default:
	error("showTree has unknown node type");
	break;
  }
}


void freeTree(Node *tree)
{
  if (tree == NULL) {
    error("freeTree has empty tree");
  }
  switch (tree->type) {
    case N_SYMBOL:
	free(tree->all.symbol.name);
	break;
    case N_INTNUM:
	break;
    case N_FLONUM:
	break;
    case N_STRING:
	free(tree->all.string.value);
	break;
    case N_CHARCON:
	break;
    case N_ARRAY:
	freeList(tree->all.array.elements);
	break;
    case N_VARIABLE:
	/* variable table records are freed separately */
	break;
    case N_BLOCK:
	freeList(tree->all.block.variables);
	freeList(tree->all.block.statements);
	break;
    case N_MESSAGE:
	if (tree->all.message.receiver != NULL) {
	  /* cascaded messages have a null receiver */
	  freeTree(tree->all.message.receiver);
	}
	freeTree(tree->all.message.selector);
	freeList(tree->all.message.arguments);
	break;
    case N_CASCADE:
	freeTree(tree->all.cascade.receiver);
	freeList(tree->all.cascade.messages);
	break;
    case N_ASSIGN:
	freeList(tree->all.assign.variables);
	freeTree(tree->all.assign.expression);
	break;
    case N_RETEXP:
	freeTree(tree->all.retexp.expression);
	break;
    case N_METHOD:
	freeTree(tree->all.method.selector);
	freeList(tree->all.method.statements);
	break;
    default:
	error("freeTree has unknown node type");
	break;
  }
  free(tree);
}
