/*
 * compiler.h -- MiniTalk-to-bytecode compiler
 */


#ifndef _COMPILER_H_
#define _COMPILER_H_


extern Bool debugTree;


void compile(char *aString, ObjPtr aClass, Bool valueNeeded);


#endif /* _COMPILER_H_ */
