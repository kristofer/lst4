/*
	memory management for the Little Smalltalk system
	Uses a variation on the Baker two-space algorithm

	Written by Tim Budd, Oregon State University, 1994
	Comments to budd@cs.orst.edu
	All rights reserved, no guarantees given whatsoever.
	May be freely redistributed if not for profit.
*/

/*
	The fundamental data type is the object.
	The first field in an object is a size, the low order two
		bits being used to maintain:
			* binary flag, used if data is binary
			* indirection flag, used if object has been relocated
	The next data field is the class
	The following fields are either objects, or character values

	The initial image is loaded into static memory space --
	space which is never garbage collected
	This improves speed, as these items are not moved during GC
*/
//#include <sys/types.h>

// struct object {
// 	uint size;
// 	struct object *class;
// 	struct object *data[0];
// };

// /*
// 	byte objects are used to represent strings and symbols
// 		bytes per word must be correct
// */

// struct byteObject {
// 	uint size;
// 	struct object *class;
// 	unsigned char bytes[0];
// };

// # define BytesPerWord 4 //maybe 2?
// # define bytePtr(x) (((struct byteObject *) x)->bytes)
// #define WORDSUP(ptr, amt) ((struct object *)(((char *)(ptr)) + \
// 	((amt) * BytesPerWord)))
// #define WORDSDOWN(ptr, amt) WORDSUP(ptr, 0 - (amt))

// /*
//  * SmallInt objects are used to represent short integers.  They are
//  * encoded as 31 bits, signed, with the low bit set to 1.  This
//  * distinguishes them from all other objects, which are longword
//  * aligned and are proper C memory pointers.
//  */
//  // this may need to be modified to 24 bits, ala Agon ADL
//  // actually maybe 16?
// #include <limits.h>

// #define IS_SMALLINT(x) ((((int)(x)) & 0x01) != 0)
// #define FITS_SMALLINT(x) ((((int)(x)) >= INT_MIN/2) && \
// 	(((int)(x)) <= INT_MAX/2))
// #define CLASS(x) (IS_SMALLINT(x) ? SmallIntClass : ((x)->class))
// #define integerValue(x) (((int)(x)) >> 1)
// #define newInteger(x) ((struct object *)((((int)(x)) << 1) | 0x01))

// /*
//  * The "size" field is the top 30 bits; the bottom two are flags
//  */
//  // here this will need to be 24 bits as well.
// #define SIZE(op) ((op)->size >> 2)
// #define SETSIZE(op, val) ((op)->size = ((val) << 2))
// #define FLAG_GCDONE (0x01)
// #define FLAG_BIN (0x02)

// /*
// 	memoryBase holds the pointer to the current space,
// 	memoryPointer is the pointer into this space.
// 	To allocate, decrement memoryPointer by the correct amount.
// 	If the result is less than memoryBase, then garbage collection
// 	must take place

// */

// extern struct object *memoryPointer, *memoryBase;

// /*
// 	roots for the memory space
// 	these are traced down during memory management
// 	rootStack is the dynamic stack
// 	staticRoots are values in static memory that point to
// 	dynamic values
// */
// # define ROOTSTACKLIMIT 50
// extern struct object *rootStack[];
// extern int rootTop;
// extern void addStaticRoot(struct object **);

// /*
// 	The following are roots for the file out
// */

// extern struct object *nilObject, *trueObject,
// 	*falseObject, *SmallIntClass, *ArrayClass, *BlockClass,
// 	*ContextClass, *globalsObject, *initialMethod,
// 	*binaryMessages[3], *IntegerClass, *badMethodSym;

// /*
// 	entry points
// */

// extern void gcinit(int, int);
// extern struct object *gcollect(int), *staticAllocate(int),
// 	*staticIAllocate(int), *gcialloc(int);
// extern void exchangeObjects(struct object *, struct object *, uint);

// extern int isDynamicMemory(struct object *);

// #define gcalloc(sz) (((memoryPointer = WORDSDOWN(memoryPointer, (sz) + 2)) < \
// 	memoryBase) ? gcollect(sz) : \
// 	(SETSIZE(memoryPointer, (sz)), memoryPointer))

// #ifndef gcalloc
// extern struct object *gcalloc(int);
// #endif
/*
    simple_memory.h - Simplified memory management for Agon Light

    Adapts the Little Smalltalk memory system for 16-bit architecture
*/

#ifndef SIMPLE_MEMORY_H
#define SIMPLE_MEMORY_H

#include <stdio.h>

/* Basic types for Z80 architecture */
typedef unsigned int uint;      /* 16-bit on Z80 */
typedef unsigned char byte;     /* 8-bit */

/* Forward declaration */
struct object;

/*
    Memory object structure - compact for Z80

    Size field is 14 bits with 2 flag bits:
    - FLAG_BIN: Object contains binary data (bytes)
    - FLAG_GCDONE: Used during garbage collection

    We use a 16-bit size field and a 16-bit class pointer
*/
struct object {
    uint size;                  /* Size and flags - 16 bits */
    struct object *class;       /* Class pointer - 16 bits */
    struct object *data[0];     /* Variable data */
};

/* Binary objects (for strings, etc.) */
struct byteObject {
    uint size;                  /* Size and flags - 16 bits */
    struct object *class;       /* Class pointer - 16 bits */
    byte bytes[0];              /* Variable-length byte array */
};

/* Architecture constants */
#define BytesPerWord 2          /* 16 bits = 2 bytes on Z80 */

/* Macros for accessing object properties */
#define bytePtr(x) (((struct byteObject *) x)->bytes)
#define WORDSUP(ptr, amt) ((struct object *)(((char *)(ptr)) + \
    ((amt) * BytesPerWord)))
#define WORDSDOWN(ptr, amt) WORDSUP(ptr, 0 - (amt))

/*
    SmallInt: 15-bit signed integers with 1 tag bit
    For Z80, we use 15 bits plus 1 tag bit
*/
#define IS_SMALLINT(x) ((((int)(x)) & 0x01) != 0)
#define FITS_SMALLINT(x) ((x) >= -16384 && (x) <= 16383)
#define CLASS(x) (IS_SMALLINT(x) ? SmallIntClass : ((x)->class))
#define integerValue(x) (((int)(x)) >> 1)
#define newInteger(x) ((struct object *)((((int)(x)) << 1) | 0x01))

/* Size and flag macros */
#define FLAG_GCDONE  0x01
#define FLAG_BIN     0x02
#define SIZE(op)     ((op)->size >> 2)
#define SETSIZE(op, val) ((op)->size = ((val) << 2))

/* Memory spaces - smaller for Z80 */
extern struct object *memoryPointer, *memoryBase;

/* Garbage collection roots */
#define ROOTSTACKLIMIT 25  /* Reduced from 50 for Z80 */
extern struct object *rootStack[];
extern int rootTop;

/* Important objects */
extern struct object *nilObject, *trueObject, *falseObject;
extern struct object *SmallIntClass, *ArrayClass, *StringClass;
extern struct object *BlockClass, *ContextClass;
extern struct object *globalsObject, *initialMethod, *badMethodSym;

/* Memory management functions */
extern void memoryInit(int staticSize, int dynamicSize);
extern struct object *gcollect(int size);
extern struct object *staticAllocate(int size);
extern struct object *staticIAllocate(int size);
extern struct object *gcialloc(int size);
extern int isDynamicMemory(struct object *obj);

/* Inline allocation with garbage collection fallback */
#define gcalloc(sz) (((memoryPointer = WORDSDOWN(memoryPointer, (sz) + 2)) < \
    memoryBase) ? gcollect(sz) : \
    (SETSIZE(memoryPointer, (sz)), memoryPointer))

#endif /* SIMPLE_MEMORY_H */
