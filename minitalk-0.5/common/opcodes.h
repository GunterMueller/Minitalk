/*
 * opcodes -- the virtual machine's operation codes
 */


#ifndef _OPCODES_H_
#define _OPCODES_H_


#define HI4(x)			((x) >> 4)
#define LO4(x)			((x) & 0x0F)


#define OP_SPECIALS		0x00
#define OP_PUSHLTRL		0x10
#define OP_PUSHINST		0x20
#define OP_STOREINST		0x30
#define OP_PUSHTEMP		0x40
#define OP_STORETEMP		0x50
#define OP_PUSHASSOC		0x60
#define OP_STOREASSOC		0x70
#define OP_SEND			0x80
#define OP_SENDSUPER		0x90
#define OP_JUMPS		0xA0
#define OP_PUSHBLOCK		0xB0
#define OP_UNUSED_Cx		0xC0
#define OP_UNUSED_Dx		0xD0
#define OP_UNUSED_Ex		0xE0
#define OP_EXTENDED		0xF0

#define OP_NOP			0x00
#define OP_PUSHSELF		0x01
#define OP_PUSHNIL		0x02
#define OP_PUSHFALSE		0x03
#define OP_PUSHTRUE		0x04
#define OP_DUP			0x05
#define OP_POP			0x06
#define OP_RET			0x07
#define OP_RETBLOCK		0x08

#define OP_JUMP			0xA0


#endif /* _OPCODES_H_ */
