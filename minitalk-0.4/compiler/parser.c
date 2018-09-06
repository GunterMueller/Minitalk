/*
 * parser.c -- MiniTalk parser
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "common.h"
#include "tree.h"
#include "scanner.h"
#include "parser.h"


Bool debugParser = false;


static Node *parseExpression(Variable **variables);
static Node *parseBlock(Variable **variables);


/*------------------------------------*/
/* Primary                            */
/*------------------------------------*/


static Node *parseSymbol(void) {
  Node *symbol;

  if (debugParser) {
    printf("ENTER parseSymbol\n");
  }
  symbol = newNode(N_SYMBOL);
  if (tokenType == T_IDENT ||
      tokenType == T_BINSEL ||
      tokenType == T_KEYWORD ||
      tokenType == T_KEYWORDS) {
    symbol->u.symbol.name = copyString(stringBuffer);
    nextToken();
  } else {
    parseError("ill formed symbol");
  }
  return symbol;
}


static Node *parseArray(void) {
  List *elements;
  int numberElements;
  Node *element;
  Node *array;

  if (debugParser) {
    printf("ENTER parseArray\n");
  }
  /* skip left parenthesis */
  nextToken();
  /* parse array elements */
  elements = NULL;
  numberElements = 0;
  while (tokenType != T_RPAREN) {
    if (tokenType == T_INTNUM) {
      element = newNode(N_INTNUM);
      element->u.intnum.value = integerBuffer;
      nextToken();
    } else
    if (tokenType == T_FLONUM) {
      element = newNode(N_FLONUM);
      element->u.flonum.value = floatBuffer;
      nextToken();
    } else
    if (tokenType == T_STRING) {
      element = newNode(N_STRING);
      element->u.string.value = copyString(stringBuffer);
      nextToken();
    } else
    if (tokenType == T_CHARCON) {
      element = newNode(N_CHARCON);
      element->u.charcon.value = charBuffer;
      nextToken();
    } else
    if (tokenType == T_LPAREN) {
      element = parseArray();
    } else {
      element = parseSymbol();
    }
    elements = appendElement(elements, element);
    numberElements++;
  }
  /* skip right parenthesis */
  nextToken();
  /* return array node */
  array = newNode(N_ARRAY);
  array->u.array.elements = elements;
  array->u.array.numberElements = numberElements;
  return array;
}


static Node *parseLiteral(void) {
  Node *literal;

  if (debugParser) {
    printf("ENTER parseLiteral\n");
  }
  if (tokenType == T_INTNUM) {
    literal = newNode(N_INTNUM);
    literal->u.intnum.value = integerBuffer;
    nextToken();
  } else
  if (tokenType == T_FLONUM) {
    literal = newNode(N_FLONUM);
    literal->u.flonum.value = floatBuffer;
    nextToken();
  } else
  if (tokenType == T_STRING) {
    literal = newNode(N_STRING);
    literal->u.string.value = copyString(stringBuffer);
    nextToken();
  } else
  if (tokenType == T_CHARCON) {
    literal = newNode(N_CHARCON);
    literal->u.charcon.value = charBuffer;
    nextToken();
  } else
  if (tokenType == T_HASH) {
    nextToken();
    if (tokenType == T_LPAREN) {
      literal = parseArray();
    } else {
      literal = parseSymbol();
    }
  } else {
    parseError("ill formed literal");
  }
  return literal;
}


static Node *parsePrimary(Variable **variables) {
  Node *primary;
  Variable *variable;

  if (debugParser) {
    printf("ENTER parsePrimary\n");
  }
  if (tokenType == T_IDENT) {
    variable = lookupVariable(stringBuffer, variables);
    if (variable == NULL) {
      parseError("unknown variable");
    }
    primary = newNode(N_VARIABLE);
    primary->u.variable.record = variable;
    nextToken();
  } else
  if (tokenType == T_LBRACK) {
    primary = parseBlock(variables);
  } else
  if (tokenType == T_LPAREN) {
    nextToken();
    primary = parseExpression(variables);
    if (tokenType != T_RPAREN) {
      parseError("unbalanced parentheses");
    }
    nextToken();
  } else {
    primary = parseLiteral();
  }
  return primary;
}


/*------------------------------------*/
/* Expression                         */
/*------------------------------------*/


static Node *parseUnaryContinuation(Node *primary) {
  Node *expression;
  Node *selector;
  Node *message;

  if (debugParser) {
    printf("ENTER parseUnaryContinuation\n");
  }
  expression = primary;
  while (tokenType == T_IDENT) {
    selector = newNode(N_SYMBOL);
    selector->u.symbol.name = copyString(stringBuffer);
    message = newNode(N_MESSAGE);
    message->u.message.receiver = expression;
    message->u.message.superFlag = isSuper(expression);
    message->u.message.selector = selector;
    nextToken();
    message->u.message.arguments = NULL;
    expression = message;
  }
  return expression;
}


static Node *parseBinaryContinuation(Node *primary, Variable **variables) {
  Node *expression;
  Node *selector;
  Node *message;

  if (debugParser) {
    printf("ENTER parseBinaryContinuation\n");
  }
  expression = parseUnaryContinuation(primary);
  while (tokenType == T_BINSEL) {
    selector = newNode(N_SYMBOL);
    selector->u.symbol.name = copyString(stringBuffer);
    message = newNode(N_MESSAGE);
    message->u.message.receiver = expression;
    message->u.message.superFlag = isSuper(expression);
    message->u.message.selector = selector;
    nextToken();
    primary = parsePrimary(variables);
    expression = parseUnaryContinuation(primary);
    message->u.message.arguments = appendElement(NULL, expression);
    expression = message;
  }
  return expression;
}


static Node *parseKeywordContinuation(Node *primary, Variable **variables) {
  Node *expression;
  Node *selector;
  Node *message;

  if (debugParser) {
    printf("ENTER parseKeywordContinuation\n");
  }
  expression = parseBinaryContinuation(primary, variables);
  if (tokenType == T_KEYWORD) {
    selector = newNode(N_SYMBOL);
    selector->u.symbol.name = NULL;
    message = newNode(N_MESSAGE);
    message->u.message.receiver = expression;
    message->u.message.superFlag = isSuper(expression);
    message->u.message.selector = selector;
    message->u.message.arguments = NULL;
    while (tokenType == T_KEYWORD) {
      selector->u.symbol.name =
	appendString(selector->u.symbol.name, stringBuffer);
      nextToken();
      primary = parsePrimary(variables);
      expression = parseBinaryContinuation(primary, variables);
      message->u.message.arguments =
	appendElement(message->u.message.arguments, expression);
    }
    expression = message;
  }
  return expression;
}


static Node *parseCascadeContinuation(Node *primary, Variable **variables) {
  Node *expression;
  Node *cascade;
  Node *selector;
  Node *message;
  Bool superFlag;

  if (debugParser) {
    printf("ENTER parseCascadeContinuation\n");
  }
  expression = parseKeywordContinuation(primary, variables);
  if (tokenType == T_SEMIC) {
    /* left hand side of cascade must be a message */
    if (expression->type != N_MESSAGE) {
      parseError("left hand side of cascade must be a message");
    }
    cascade = newNode(N_CASCADE);
    cascade->u.cascade.receiver = expression->u.message.receiver;
    superFlag = isSuper(expression->u.message.receiver);
    expression->u.message.receiver = NULL;
    cascade->u.cascade.messages = appendElement(NULL, expression);
    while (tokenType == T_SEMIC) {
      selector = newNode(N_SYMBOL);
      message = newNode(N_MESSAGE);
      message->u.message.receiver = NULL;
      message->u.message.superFlag = superFlag;
      message->u.message.selector = selector;
      nextToken();
      if (tokenType == T_IDENT) {
	selector->u.symbol.name = copyString(stringBuffer);
	message->u.message.arguments = NULL;
	nextToken();
      } else
      if (tokenType == T_BINSEL) {
	selector->u.symbol.name = copyString(stringBuffer);
	nextToken();
	primary = parsePrimary(variables);
	expression = parseUnaryContinuation(primary);
	message->u.message.arguments = appendElement(NULL, expression);
      } else
      if (tokenType == T_KEYWORD) {
	selector->u.symbol.name = NULL;
	message->u.message.arguments = NULL;
	while (tokenType == T_KEYWORD) {
	  selector->u.symbol.name =
	    appendString(selector->u.symbol.name, stringBuffer);
	  nextToken();
	  primary = parsePrimary(variables);
	  expression = parseBinaryContinuation(primary, variables);
	  message->u.message.arguments =
	    appendElement(message->u.message.arguments, expression);
	}
      } else {
	parseError("ill formed cascade");
      }
      cascade->u.cascade.messages =
	appendElement(cascade->u.cascade.messages, message);
    }
    expression = cascade;
  }
  return expression;
}


static Node *parseExpression(Variable **variables) {
  Node *primary;
  Node *expression;

  if (debugParser) {
    printf("ENTER parseExpression\n");
  }
  primary = parsePrimary(variables);
  if (tokenType == T_ASSIGN) {
    expression = newNode(N_ASSIGN);
    expression->u.assign.variables = NULL;
    while (tokenType == T_ASSIGN) {
      /* left hand side of assignment must be a variable */
      if (primary->type != N_VARIABLE) {
	parseError("left hand side of assignment must be a variable");
      }
      /* and it must be an assignable one */
      if (!isAssignable(primary->u.variable.record)) {
	parseError("this variable is not assignable");
      }
      expression->u.assign.variables =
	appendElement(expression->u.assign.variables, primary);
      nextToken();
      primary = parsePrimary(variables);
    }
    expression->u.assign.expression =
      parseCascadeContinuation(primary, variables);
  } else {
    expression = parseCascadeContinuation(primary, variables);
  }
  return expression;
}


/*------------------------------------*/
/* Statements                         */
/*------------------------------------*/


static List *parseStatements(Variable **variables) {
  List *statements;
  Node *expression;

  if (debugParser) {
    printf("ENTER parseStatements\n");
  }
  statements = NULL;
  while (tokenType != T_RBRACK && tokenType != T_END) {
    if (tokenType == T_CARET) {
      nextToken();
      expression = newNode(N_RETEXP);
      expression->u.retexp.expression = parseExpression(variables);
      statements = appendElement(statements, expression);
      break;
    }
    expression = parseExpression(variables);
    statements = appendElement(statements, expression);
    if (tokenType == T_RBRACK || tokenType == T_END) {
      break;
    }
    if (tokenType != T_PERIOD) {
      parseError("statement separator '.' expected");
    }
    nextToken();
  }
  return statements;
}


/*------------------------------------*/
/* Block                              */
/*------------------------------------*/


static Node *parseBlock(Variable **variables) {
  Node *block;
  Variable *variable;
  Node *variableNode;

  if (debugParser) {
    printf("ENTER parseBlock\n");
  }
  block = newNode(N_BLOCK);
  if (tokenType != T_LBRACK) {
    parseError("block must begin with '['");
  }
  nextToken();
  block->u.block.variables = NULL;
  block->u.block.numberVariables = 0;
  if (tokenType == T_COLONVAR) {
    while (tokenType == T_COLONVAR) {
      if (!islower(stringBuffer[0])) {
	parseError("block argument name must begin with lower case letter");
      }
      /* block argument names may be declared many times */
      variable = lookupVariable(stringBuffer, variables);
      if (variable == NULL) {
	variable = enterVariable(V_TEMPORARY, stringBuffer, variables);
      }
      variableNode = newNode(N_VARIABLE);
      variableNode->u.variable.record = variable;
      block->u.block.variables =
	appendElement(block->u.block.variables, variableNode);
      block->u.block.numberVariables++;
      nextToken();
    }
    if (tokenType != T_BINSEL || strcmp(stringBuffer, "|") != 0) {
      parseError("declaration of colon variables must end with '|'");
    }
    nextToken();
  }
  block->u.block.statements = parseStatements(variables);
  if (tokenType != T_RBRACK) {
    parseError("block must end with ']'");
  }
  nextToken();
  return block;
}


/*------------------------------------*/
/* Method                             */
/*------------------------------------*/


static Node *parseMessagePattern(Variable **variables) {
  char *name;
  Node *selector;

  if (debugParser) {
    printf("ENTER parseMessagePattern\n");
  }
  if (tokenType == T_IDENT) {
    /* unary pattern */
    name = copyString(stringBuffer);
    nextToken();
  } else
  if (tokenType == T_BINSEL) {
    /* binary pattern */
    name = copyString(stringBuffer);
    nextToken();
    if (tokenType != T_IDENT || !islower(stringBuffer[0])) {
      parseError("argument name must begin with lower case letter");
    }
    if (enterVariable(V_ARGUMENT, stringBuffer, variables) == NULL) {
      parseError("variable '%s' is already defined", stringBuffer);
    }
    nextToken();
  } else
  if (tokenType == T_KEYWORD) {
    /* keyword pattern */
    name = NULL;
    while (tokenType == T_KEYWORD) {
      name = appendString(name, stringBuffer);
      nextToken();
      if (tokenType != T_IDENT || !islower(stringBuffer[0])) {
	parseError("argument name must begin with lower case letter");
      }
      if (enterVariable(V_ARGUMENT, stringBuffer, variables) == NULL) {
        parseError("variable '%s' is already defined", stringBuffer);
      }
      nextToken();
    }
  } else {
    parseError("ill formed message pattern");
  }
  /* construct symbol node containing selector */
  selector = newNode(N_SYMBOL);
  selector->u.symbol.name = name;
  return selector;
}


static void parseTemporaries(Variable **variables) {
  if (debugParser) {
    printf("ENTER parseTemporaries\n");
  }
  if (tokenType == T_BINSEL && strcmp(stringBuffer, "|") == 0) {
    nextToken();
    while (tokenType == T_IDENT) {
      if (!islower(stringBuffer[0])) {
	parseError("temporary name must begin with lower case letter");
      }
      if (enterVariable(V_TEMPORARY, stringBuffer, variables) == NULL) {
        parseError("variable '%s' is already defined", stringBuffer);
      }
      nextToken();
    }
    if (tokenType != T_BINSEL || strcmp(stringBuffer, "|") != 0) {
      parseError("declaration of temporary variables must end with '|'");
    }
    nextToken();
  }
}


static int parsePrimitive(void) {
  int primitive;

  if (debugParser) {
    printf("ENTER parsePrimitive\n");
  }
  primitive = -1;
  if (tokenType == T_BINSEL && strcmp(stringBuffer, "<") == 0) {
    nextToken();
    if (tokenType != T_INTNUM) {
      parseError("primitive must be specified by integer number");
    }
    primitive = integerBuffer;
    nextToken();
    if (tokenType != T_BINSEL || strcmp(stringBuffer, ">") != 0) {
      parseError("primitive specification must end with '>'");
    }
    nextToken();
  }
  return primitive;
}


Node *parseMethod(Variable **variables) {
  Node *method;

  if (debugParser) {
    printf("ENTER parseMethod\n");
  }
  method = newNode(N_METHOD);
  method->u.method.selector = parseMessagePattern(variables);
  parseTemporaries(variables);
  method->u.method.primitive = parsePrimitive();
  method->u.method.statements = parseStatements(variables);
  return method;
}
