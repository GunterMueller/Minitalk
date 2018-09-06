/*
 * scanner.h -- MiniTalk scanner
 */


#ifndef _SCANNER_H_
#define _SCANNER_H_


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


extern Bool debugScanner;

extern int tokenType;
extern char stringBuffer[MAX_CHUNKSIZE];
extern char charBuffer;
extern long integerBuffer;
extern double floatBuffer;


void parseError(char *fmt, ...);
void initScanner(char *text);
void nextToken(void);


#endif /* _SCANNER_H_ */
