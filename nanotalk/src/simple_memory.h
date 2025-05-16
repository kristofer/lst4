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
