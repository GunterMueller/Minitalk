/*
 * scanner.c -- MiniTalk scanner
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>

#include "common.h"
#include "utils.h"
#include "scanner.h"


/*------------------------------------*/
/* Global Variables                   */
/*------------------------------------*/


Bool debugScanner = false;

static char *source;
static char *curPos;
static int lineNo;

int tokenType;
char stringBuffer[MAX_CHUNKSIZE];
char charBuffer;
long integerBuffer;
double floatBuffer;


/*------------------------------------*/
/* Error Handling                     */
/*------------------------------------*/


void parseError(char *fmt, ...) {
  char *lp;
  int ln;
  char *cp;
  va_list ap;

  /* skip source lines to line which contains error */
  lp = source;
  ln = 0;
  while (ln < lineNo) {
    while (*lp++ != '\n') ;
    ln++;
  }
  /* show source line which contains error */
  putchar('\n');
  cp = lp;
  while (*cp != '\0' && *cp != '\n') {
    putchar(*cp);
    cp++;
  }
  /* indicate position of error in line */
  putchar('\n');
  cp = lp;
  while (cp != curPos) {
    if (*cp == '\t') {
      putchar('\t');
    } else {
      putchar(' ');
    }
    cp++;
  }
  putchar('^');
  /* show error message */
  va_start(ap, fmt);
  error(fmt, ap);
  va_end(ap);
}


/*------------------------------------*/
/* Character Handling                 */
/*------------------------------------*/


static char nextChar(void) {
  if (*curPos == '\0') {
    return '\0';
  }
  return *curPos++;
}


static void backChar(char c) {
  if (c == '\0') {
    return;
  }
  *--curPos = c;
}


/*------------------------------------*/
/* Scanner                            */
/*------------------------------------*/


static Bool isSpecial(char c) {
  return c == '+' ||
	 c == '/' ||
	 c == '\\' ||
	 c == '*' ||
	 c == '~' ||
	 c == '<' ||
	 c == '>' ||
	 c == '=' ||
	 c == '@' ||
	 c == '%' ||
	 c == '|' ||
	 c == '&' ||
	 c == '?' ||
	 c == '!' ||
	 c == ',';
}


static Bool isValidDigit(char c, int base) {
  if (isdigit(c)) {
    return (c - '0') < base;
  }
  if ('A' <= c && c <= 'Z') {
    return (c - 'A' + 10) < base;
  }
  return false;
}


static int digitValue(char c) {
  if (isdigit(c)) {
    return c - '0';
  } else {
    return c - 'A' + 10;
  }
}


static void getToken(void) {
  char c;
  char *cp;
  Bool negative;
  int base;
  double scale;

  while (1) {
    c = nextChar();
    /* white space? */
    if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
      if (c == '\n') {
	lineNo++;
      }
      continue;
    }
    /* comment? */
    if (c == '"') {
      do {
	do {
	  c = nextChar();
	  if (c == '\0') {
	    parseError("unterminated comment");
	  }
	  if (c == '\n') {
	    lineNo++;
	  }
	} while (c != '"') ;
	c = nextChar();
      } while (c == '"') ;
      backChar(c);
      continue;
    }
    /* end of input? */
    if (c == '\0') {
      tokenType = T_END;
      return;
    }
    /* identifier, keyword, keywords? */
    if (isalpha(c)) {
      cp = stringBuffer;
      while (isalnum(c)) {
	*cp++ = c;
	c = nextChar();
      }
      if (c != ':') {
	/* identifier */
	backChar(c);
	*cp = '\0';
	tokenType = T_IDENT;
	return;
      }
      /* keyword, keywords */
      *cp++ = c;
      c = nextChar();
      if (!isalpha(c)) {
	/* keyword */
	backChar(c);
	*cp = '\0';
	tokenType = T_KEYWORD;
	return;
      }
      /* keywords */
      while (isalpha(c)) {
	while (isalnum(c)) {
	  *cp++ = c;
	  c = nextChar();
	}
	if (c != ':') {
	  parseError("keyword must be followed by colon");
	}
	*cp++ = c;
	c = nextChar();
      }
      backChar(c);
      *cp = '\0';
      tokenType = T_KEYWORDS;
      return;
    }
    /* colon variable? */
    if (c == ':') {
      c = nextChar();
      if (!isalpha(c)) {
	parseError("colon variable must start with a letter");
      }
      cp = stringBuffer;
      while (isalnum(c)) {
	*cp++ = c;
	c = nextChar();
      }
      backChar(c);
      *cp = '\0';
      tokenType = T_COLONVAR;
      return;
    }
    /* binary selector, assign? */
    if (isSpecial(c)) {
      stringBuffer[0] = c;
      c = nextChar();
      if (stringBuffer[0] == '<' && c == '-') {
	tokenType = T_ASSIGN;
	return;
      }
      if (!isSpecial(c)) {
	backChar(c);
	stringBuffer[1] = '\0';
      } else {
	stringBuffer[1] = c;
	stringBuffer[2] = '\0';
      }
      tokenType = T_BINSEL;
      return;
    }
    /* string? */
    if (c == '\'') {
      cp = stringBuffer;
      do {
	do {
	  c = nextChar();
	  if (c == '\0') {
	    parseError("unterminated string");
	  }
	  if (c == '\n') {
	    lineNo++;
	  }
	  *cp++ = c;
	} while (c != '\'') ;
	c = nextChar();
      } while (c == '\'') ;
      backChar(c);
      *--cp = '\0';
      tokenType = T_STRING;
      return;
    }
    /* character constant? */
    if (c == '$') {
      charBuffer = nextChar();
      tokenType = T_CHARCON;
      return;
    }
    /* integer or floating number, binary selector '-'? */
    if (c == '-' || isdigit(c)) {
      /* handle sign; maybe binary selector */
      if (c == '-') {
	c = nextChar();
	if (!isdigit(c)) {
	  /* binary selector */
	  backChar(c);
	  stringBuffer[0] = '-';
	  stringBuffer[1] = '\0';
	  tokenType = T_BINSEL;
	  return;
	}
	negative = true;
      } else {
	negative = false;
      }
      /* now read integer part or base */
      integerBuffer = 0;
      if (!isdigit(c)) {
	parseError("number must start with a decimal digit");
      }
      while (isdigit(c)) {
	integerBuffer *= 10;
	integerBuffer += c - '0';
	c = nextChar();
      }
      if (c == 'r') {
	/* number just read is base */
	base = integerBuffer;
	if (negative) {
	  parseError("number base must not be negative");
	}
	if (base < 2 || base > 36) {
	  parseError("number base must be between 2 and 36");
	}
	/* handle sign */
	c = nextChar();
	if (c == '-') {
	  c = nextChar();
	  negative = true;
	} else {
	  negative = false;
	}
	/* now read integer part of number */
	integerBuffer = 0;
	if (!isValidDigit(c, base)) {
	  parseError("number must start with a valid digit");
	}
	while (isValidDigit(c, base)) {
	  integerBuffer *= base;
	  integerBuffer += digitValue(c);
	  c = nextChar();
	}
      } else {
	/* base is implicitly 10 */
	base = 10;
      }
      /* here the integer part has definitely been read */
      /* integer number? */
      if (c != '.' && c != 'e') {
	backChar(c);
	if (negative) {
	  integerBuffer = -integerBuffer;
	}
	tokenType = T_INTNUM;
	return;
      }
      /* integer number followed by period or fractional part? */
      floatBuffer = integerBuffer;
      if (c == '.') {
	c = nextChar();
	/* integer number followed by period? */
	if (!isValidDigit(c, base)) {
	  backChar(c);
	  backChar('.');
	  if (negative) {
	    integerBuffer = -integerBuffer;
	  }
	  tokenType = T_INTNUM;
	  return;
	}
	/* fractional part */
	scale = 1.0;
	while (isValidDigit(c, base)) {
	  scale /= base;
	  floatBuffer += digitValue(c) * scale;
	  c = nextChar();
	}
      }
      if (negative) {
	floatBuffer = -floatBuffer;
      }
      /* exponent part? */
      if (c == 'e') {
	/* handle sign */
	c = nextChar();
	if (c == '-') {
	  c = nextChar();
	  negative = true;
	} else {
	  negative = false;
	}
	/* now read exponent part of number */
	integerBuffer = 0;
	if (!isdigit(c)) {
	  parseError("exponent must start with a decimal digit");
	}
	while (isdigit(c)) {
	  integerBuffer *= 10;
	  integerBuffer += c - '0';
	  c = nextChar();
	}
	if (negative) {
	  integerBuffer = -integerBuffer;
	}
	floatBuffer *= pow(base, integerBuffer);
      }
      backChar(c);
      tokenType = T_FLONUM;
      return;
    }
    /* other tokens */
    switch (c) {
      case '(':
	tokenType = T_LPAREN;
	return;
      case ')':
	tokenType = T_RPAREN;
	return;
      case '[':
	tokenType = T_LBRACK;
	return;
      case ']':
	tokenType = T_RBRACK;
	return;
      case '.':
	tokenType = T_PERIOD;
	return;
      case ';':
	tokenType = T_SEMIC;
	return;
      case '^':
	tokenType = T_CARET;
	return;
      case '#':
	tokenType = T_HASH;
	return;
      default:
	parseError("illegal character");
	break;
    }
  }
}


/*------------------------------------*/
/* Token Display                      */
/*------------------------------------*/


static void showToken(void) {
  switch (tokenType) {
    case T_END:
	printf("T_END\n");
	break;
    case T_IDENT:
	printf("T_IDENT\t\t= %s\n", stringBuffer);
	break;
    case T_KEYWORD:
	printf("T_KEYWORD\t= %s\n", stringBuffer);
	break;
    case T_KEYWORDS:
	printf("T_KEYWORDS\t= %s\n", stringBuffer);
	break;
    case T_COLONVAR:
	printf("T_COLONVAR\t= %s\n", stringBuffer);
	break;
    case T_BINSEL:
	printf("T_BINSEL\t= %s\n", stringBuffer);
	break;
    case T_ASSIGN:
	printf("T_ASSIGN\n");
	break;
    case T_STRING:
	printf("T_STRING\t= %s\n", stringBuffer);
	break;
    case T_CHARCON:
	printf("T_CHARCON\t= %c\n", charBuffer);
	break;
    case T_INTNUM:
	printf("T_INTNUM\t= %ld\n", integerBuffer);
	break;
    case T_FLONUM:
	printf("T_FLONUM\t= %e\n", floatBuffer);
	break;
    case T_LPAREN:
	printf("T_LPAREN\n");
	break;
    case T_RPAREN:
	printf("T_RPAREN\n");
	break;
    case T_LBRACK:
	printf("T_LBRACK\n");
	break;
    case T_RBRACK:
	printf("T_RBRACK\n");
	break;
    case T_PERIOD:
	printf("T_PERIOD\n");
	break;
    case T_SEMIC:
	printf("T_SEMIC\n");
	break;
    case T_CARET:
	printf("T_CARET\n");
	break;
    case T_HASH:
	printf("T_HASH\n");
	break;
    default:
	error("showToken has unknown token");
	break;
  }
}


/*------------------------------------*/
/* Scanner Interface                  */
/*------------------------------------*/


void initScanner(char *text) {
  source = text;
  curPos = source;
  lineNo = 0;
}


void nextToken(void) {
  getToken();
  if (debugScanner) {
    showToken();
  }
}
