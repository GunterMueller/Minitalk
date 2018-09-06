/*
 * primmeth.h -- primitive methods
 */


#ifndef _PRIMMETH_H_
#define _PRIMMETH_H_


typedef Bool (*PrimitiveMethod)(int numberArguments);


extern PrimitiveMethod primitiveMethods[256];


#endif /* _PRIMMETH_H_ */
