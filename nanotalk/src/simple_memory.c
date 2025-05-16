/*
    simple_memory.c - Simplified memory management for Agon Light

    Implements basic two-space garbage collection algorithm
    optimized for Z80 16-bit architecture
*/

#include "simple_memory.h"
#include <stdlib.h>
#include <string.h>

/* Memory spaces */
static struct object *staticBase, *staticTop, *staticPointer;
static struct object *spaceOne, *spaceTwo;
static int spaceSize;
struct object *memoryBase, *memoryPointer, *memoryTop;

/* Garbage collection state */
static int inSpaceOne;
static struct object *oldBase, *oldTop;

/* Root stacks for garbage collection */
struct object *rootStack[ROOTSTACKLIMIT];
int rootTop = 0;

#define STATICROOTLIMIT 50  /* Reduced for Z80 */
static struct object **staticRoots[STATICROOTLIMIT];
static int staticRootTop = 0;

/* Counter for garbage collections */
int gcCount = 0;

/*
    Test if a pointer is in dynamic memory or not
*/
int
isDynamicMemory(struct object *x)
{
    return ((x >= spaceOne) && (x <= (spaceOne + spaceSize))) ||
           ((x >= spaceTwo) && (x <= (spaceTwo + spaceSize)));
}

/*
    Initialize memory system with static and dynamic areas
*/
void
memoryInit(int staticSize, int dynamicSize)
{
    /* Allocate memory spaces */
    staticBase = (struct object *)
        malloc(staticSize * sizeof(struct object));
    spaceOne = (struct object *)
        malloc(dynamicSize * sizeof(struct object));
    spaceTwo = (struct object *)
        malloc(dynamicSize * sizeof(struct object));

    if ((staticBase == 0) || (spaceOne == 0) || (spaceTwo == 0)) {
        fprintf(stderr, "Not enough memory for space allocations\n");
        exit(1);
    }

    /* Set up static memory */
    staticTop = staticBase + staticSize;
    staticPointer = staticTop;

    /* Set up dynamic memory */
    spaceSize = dynamicSize;
    memoryBase = spaceOne;
    memoryPointer = memoryBase + spaceSize;
    inSpaceOne = 1;
}

/*
    Core garbage collection function - simplified from Baker's algorithm
    Moves objects from old space to new space
*/
static struct object *
gc_move(struct object *ptr)
{
    struct object *old_address, *new_address, *result;
    uint size, i;

    /* Handle SmallInts (not real pointers) */
    if (IS_SMALLINT(ptr)) {
        return ptr;
    }

    /* Check if pointer is outside our spaces */
    if ((ptr < oldBase) || (ptr > oldTop)) {
        return ptr;
    }

    /* Check if object already moved (has forwarding pointer) */
    if (ptr->size & FLAG_GCDONE) {
        if (ptr->size & FLAG_BIN) {
            result = (struct object *)ptr->data[0];
        } else {
            size = SIZE(ptr);
            result = ptr->data[size];
        }
        return result;
    }

    /* Move the object to new space */
    if (ptr->size & FLAG_BIN) {
        /* Binary object */
        int byteSize = SIZE(ptr);
        int wordSize = (byteSize + BytesPerWord - 1) / BytesPerWord;

        memoryPointer = WORDSDOWN(memoryPointer, wordSize + 2);
        new_address = memoryPointer;
        SETSIZE(new_address, byteSize);
        new_address->size |= FLAG_BIN;

        /* Copy binary data */
        memcpy(bytePtr(new_address), bytePtr(ptr), byteSize);

        /* Copy class pointer (and fix it up later) */
        new_address->class = ptr->class;

        /* Mark old object as moved */
        ptr->size |= FLAG_GCDONE;
        ptr->data[0] = (struct object *)new_address;

        /* Now move the class */
        new_address->class = gc_move(new_address->class);

        return new_address;
    } else {
        /* Regular object */
        size = SIZE(ptr);
        memoryPointer = WORDSDOWN(memoryPointer, size + 2);
        new_address = memoryPointer;
        SETSIZE(new_address, size);

        /* Mark old object as moved with forwarding pointer */
        ptr->size |= FLAG_GCDONE;
        ptr->data[size] = new_address;

        /* Copy class pointer (and fix it up later) */
        new_address->class = ptr->class;

        /* Copy and move all instance variables */
        for (i = 0; i < size; i++) {
            new_address->data[i] = gc_move(ptr->data[i]);
        }

        return new_address;
    }
}

/*
    Garbage collection entry point
*/
struct object *
gcollect(int sz)
{
    int i;

    gcCount++;

    /* Swap spaces */
    if (inSpaceOne) {
        memoryBase = spaceTwo;
        inSpaceOne = 0;
        oldBase = spaceOne;
    } else {
        memoryBase = spaceOne;
        inSpaceOne = 1;
        oldBase = spaceTwo;
    }
    memoryPointer = memoryTop = memoryBase + spaceSize;
    oldTop = oldBase + spaceSize;

    /* Trace roots and move objects */
    for (i = 0; i < rootTop; i++) {
        rootStack[i] = gc_move(rootStack[i]);
    }
    for (i = 0; i < staticRootTop; i++) {
        (*staticRoots[i]) = gc_move(*staticRoots[i]);
    }

    /* Clear method cache if we had one */
    /* flushCache(); */

    /* Allocate requested object in new space */
    memoryPointer = WORDSDOWN(memoryPointer, sz + 2);
    if (memoryPointer < memoryBase) {
        fprintf(stderr, "Out of memory after garbage collection\n");
        exit(1);
    }
    SETSIZE(memoryPointer, sz);
    return(memoryPointer);
}

/*
    Allocate in static memory (not garbage collected)
*/
struct object *
staticAllocate(int sz)
{
    staticPointer = WORDSDOWN(staticPointer, sz + 2);
    if (staticPointer < staticBase) {
        fprintf(stderr, "Out of static memory\n");
        exit(1);
    }
    SETSIZE(staticPointer, sz);
    return(staticPointer);
}

/*
    Allocate a binary object in static memory
*/
struct object *
staticIAllocate(int sz)
{
    int trueSize;
    struct object *result;

    trueSize = (sz + BytesPerWord - 1) / BytesPerWord;
    result = staticAllocate(trueSize);
    SETSIZE(result, sz);
    result->size |= FLAG_BIN;
    return result;
}

/*
    Allocate a binary object in dynamic memory
*/
struct object *
gcialloc(int sz)
{
    int trueSize;
    struct object *result;

    trueSize = (sz + BytesPerWord - 1) / BytesPerWord;
    result = gcalloc(trueSize);
    SETSIZE(result, sz);
    result->size |= FLAG_BIN;
    return result;
}

/*
    Register a static object field that might point to dynamic objects
*/
void
addStaticRoot(struct object **objp)
{
    int i;

    /* Check if already registered */
    for (i = 0; i < staticRootTop; ++i) {
        if (objp == staticRoots[i]) {
            return;
        }
    }

    /* Add to static roots table */
    if (staticRootTop >= STATICROOTLIMIT) {
        fprintf(stderr, "Too many static references\n");
        exit(1);
    }
    staticRoots[staticRootTop++] = objp;
}
