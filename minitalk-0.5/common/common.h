/*
 * common.h -- common definitions
 */


#ifndef _COMMON_H_
#define _COMMON_H_


/* version number */

#define MAJOR_VNUM	0		/* major version number */
#define MINOR_VNUM	5		/* minor version number */


/* greeting and byebye messages */

#define GREETING	"\nMiniTalk Version %d.%d\n\n", MAJOR_VNUM, MINOR_VNUM
#define BYEBYE		"\nMiniTalk session ended\n\n"


/* default image name and standard image path */

#define DFLT_IMG_NAME	"minitalk.img"
/* #define STD_IMG_PATH	"/home/eco/minitalk/work/minitalk-0.5/build/lib" */
#define STD_IMG_PATH	"../lib"
#define STD_IMG_NAME	"minitalk-std.img"
/* #define STD_SRC_PATH	"/home/eco/minitalk/work/minitalk-0.5/build/lib" */
#define STD_SRC_PATH	"../lib"
#define STD_SRC_NAME	"minitalk-std.src"


/* some sizes */

#define K		1024
#define M		(K * K)

#define SEMI_SIZE	(300 * K)	/* size of a single semispace */
#define MEMORY_SIZE	(2 * SEMI_SIZE)	/* total size of object memory */

#define LINE_SIZE	254		/* size of input line */
#define MAX_CHUNKSIZE	10000		/* max size of a chunk of input */


/* boolean type */

typedef enum { false, true } Bool;


/* unsigned types, address type, and object pointer type */

typedef unsigned char Byte;
typedef unsigned int Word;
typedef unsigned long Address;

typedef Address ObjPtr;


/* most and next significant bits of words and object pointers */

#define WORD_MSB	(((Word) 1) << (8 * sizeof(Word) - 1))
#define WORD_NSB	(((Word) 1) << (8 * sizeof(Word) - 2))

#define OBJPTR_MSB	(((ObjPtr) 1) << (8 * sizeof(ObjPtr) - 1))
#define OBJPTR_NSB	(((ObjPtr) 1) << (8 * sizeof(ObjPtr) - 2))


#endif /* _COMMON_H_ */
