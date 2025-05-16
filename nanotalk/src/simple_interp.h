/*
    Simple Smalltalk bytecode interpreter header for Agon Light
    Based on Little Smalltalk v4
*/

#ifndef SIMPLE_INTERP_H
#define SIMPLE_INTERP_H

#include "simple_memory.h"

/* Bytecode definitions */
#define Extended 0
#define PushInstance 1
#define PushArgument 2
#define PushTemporary 3
#define PushLiteral 4
#define PushConstant 5
#define AssignInstance 6
#define AssignTemporary 7
#define MarkArguments 8
#define SendMessage 9
#define SendUnary 10
#define SendBinary 11
#define PushBlock 12
#define DoPrimitive 13
#define DoSpecial 15

/* DoSpecial operations */
#define SelfReturn 1
#define StackReturn 2
#define BlockReturn 3
#define Duplicate 4
#define PopTop 5
#define Branch 6
#define BranchIfTrue 7
#define BranchIfFalse 8
#define SendToSuper 11

/* Constants */
#define nilConst 10
#define trueConst 11
#define falseConst 12

/* Return codes */
#define ReturnError 2
#define ReturnBadMethod 3
#define ReturnReturned 4
#define ReturnTimeExpired 5

/* Object field offsets - Process */
#define contextInProcess 0
#define statusInProcess 1
#define resultInProcess 2

/* Object field offsets - Context */
#define contextSize 7
#define methodInContext 0
#define argumentsInContext 1
#define temporariesInContext 2
#define stackInContext 3
#define bytePointerInContext 4
#define stackTopInContext 5
#define previousContextInContext 6

/* Object field offsets - Method */
#define methodSize 7
#define nameInMethod 0
#define byteCodesInMethod 1
#define literalsInMethod 2
#define stackSizeInMethod 3
#define temporarySizeInMethod 4
#define classInMethod 5
#define textInMethod 6

/* Object field offsets - Class */
#define ClassSize 5
#define nameInClass 0
#define parentClassInClass 1
#define methodsInClass 2
#define instanceSizeInClass 3
#define variablesInClass 4

/* Arguments array */
#define receiverInArguments 0

/* External variable declarations */
extern unsigned char debugging;
extern unsigned int cacheHit, cacheMiss;
extern struct object *nilObject, *trueObject, *falseObject;
extern struct object *SmallIntClass, *ArrayClass, *BlockClass, *ContextClass;
extern struct object *globalsObject, *initialMethod, *badMethodSym;

/* Function prototypes */
int execute(struct object *aProcess, int ticks);
void flushCache(void);

/* Primitive function type */
typedef struct object *(*PrimitiveFn)(struct object *args, int *failed);

/* Symbol comparison utility */
static int symbolCompare(struct object *left, struct object *right);

#endif /* SIMPLE_INTERP_H */
