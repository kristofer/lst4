/*
    simple_object.h - Basic object structure definitions for Agon Light

    Defines the core classes needed for a minimal Smalltalk system
*/

#ifndef SIMPLE_OBJECT_H
#define SIMPLE_OBJECT_H

#include "simple_memory.h"

/*
    Class structure field offsets
    Compact design for Z80 architecture
*/
#define ClassSize 4
#define nameInClass 0
#define parentClassInClass 1
#define methodsInClass 2
#define instanceSizeInClass 3

/* Method structure field offsets */
#define methodSize 5
#define nameInMethod 0
#define byteCodesInMethod 1
#define literalsInMethod 2
#define stackSizeInMethod 3
#define temporarySizeInMethod 4

/* Context structure field offsets */
#define contextSize 6
#define methodInContext 0
#define argumentsInContext 1
#define temporariesInContext 2
#define stackInContext 3
#define bytePointerInContext 4
#define stackTopInContext 5

/* Dictionary structure */
#define keysInDict 0
#define valuesInDict 1

/* Method lookup */
static int symbolCompare(struct object *left, struct object *right);
struct object *lookupMethod(struct object *selector, struct object *class);

/*
    Initialize minimal object system with core classes
*/
void initializeObjectSystem(void);

/*
    Create basic Smalltalk objects
*/
struct object *newSymbol(const char *name, int length);
struct object *newString(const char *text, int length);
struct object *newClass(const char *name, struct object *superClass, int instanceSize);
struct object *newMethod(struct object *selector, int tempSize, int stackSize);
struct object *newContext(struct object *method, struct object *arguments);
struct object *newArray(int size);
struct object *newDictionary(void);

/* Important objects */
extern struct object *nilObject, *trueObject, *falseObject;
extern struct object *SmallIntClass, *ArrayClass, *StringClass;
extern struct object *BlockClass, *ContextClass;
extern struct object *globalsObject, *initialMethod, *badMethodSym;

#endif /* SIMPLE_OBJECT_H */
