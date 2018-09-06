/*
 * tree.h -- abstract syntax tree
 */


#ifndef _TREE_H_
#define _TREE_H_


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


typedef struct list {
  struct node *head;
  struct list *tail;
} List;


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
      List *elements;
      int numberElements;
    } array;
    /* N_VARIABLE */
    struct {
      Variable *record;
    } variable;
    /* N_BLOCK */
    struct {
      List *variables;
      int numberVariables;
      List *statements;
    } block;
    /* N_MESSAGE */
    struct {
      struct node *receiver;
      Bool superFlag;
      struct node *selector;
      List *arguments;
    } message;
    /* N_CASCADE */
    struct {
      struct node *receiver;
      List *messages;
    } cascade;
    /* N_ASSIGN */
    struct {
      List *variables;
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
      List *statements;
    } method;
  } u;
} Node;


char *copyString(char *string);
char *appendString(char *alreadyThere, char *toBeAppended);

Bool isSuper(Node *expression);
Bool isAssignable(Variable *variable);
Variable *lookupVariable(char *name, Variable **variables);
Variable *enterVariable(int type, char *name, Variable **variables);
void initVariables(Variable **variables, ObjPtr aClass);
void computeOffsets(Variable **variables, Node *method);
void showVariables(Variable **variables);
void freeVariables(Variable **variables);

Node *newNode(int type);
List *appendElement(List *list, Node *element);
void showTree(Node *tree);
void freeTree(Node *tree);


#endif /* _TREE_H_ */
