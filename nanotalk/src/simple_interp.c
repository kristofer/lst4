/*
    Simple Smalltalk Interpreter for Agon Light
    Based on Little Smalltalk v4 by Tim Budd

    Stripped down to essential bytecodes:
    - PushConstant
    - PushLiteral
    - SendMessage
    - DoSpecial
*/

#include "simple_memory.h"
#include "simple_interp.h"
#include <stdio.h>
#include <string.h>

/* Debugging flag */
unsigned char debugging = 0;

/* Cache statistics */
unsigned int cacheHit = 0;
unsigned int cacheMiss = 0;

/*
    Global object references used throughout the system
*/
struct object *nilObject, *trueObject, *falseObject;
struct object *SmallIntClass, *ArrayClass, *StringClass;
struct object *BlockClass, *ContextClass;
struct object *globalsObject, *initialMethod, *badMethodSym;

/*
    Method lookup - find a method in a class's method dictionary
*/
static struct object *
lookupMethod(struct object *selector, struct object *class)
{
    struct object *dict, *keys, *vals, *val;
    uint low, high, mid;

    /* Scan upward through class hierarchy */
    for (; class != nilObject; class = class->data[parentClassInClass]) {
        /* Get the method dictionary */
        dict = class->data[methodsInClass];
        keys = dict->data[0];
        low = 0;
        high = SIZE(keys);

        /* Binary search for the selector */
        while (low < high) {
            mid = (low + high) / 2;
            val = keys->data[mid];

            /* If found, return the method object */
            if (val == selector) {
                vals = dict->data[1];
                return(vals->data[mid]);
            }

            /* Adjust search range */
            if (symbolCompare(selector, val) < 0) {
                high = mid;
            } else {
                low = mid+1;
            }
        }
    }

    /* Method not found */
    return(NULL);
}

/*
    Simple method cache implementation
*/
#define cacheSize 53  /* Smaller cache for memory savings */

static struct {
    struct object *name;
    struct object *class;
    struct object *method;
} cache[cacheSize];

/* Clear the method cache */
void
flushCache(void)
{
    int i;
    for (i = 0; i < cacheSize; i++) {
        cache[i].name = 0;
    }
}

/*
    Symbol comparison - compares two Symbol objects
*/
static int
symbolCompare(struct object *left, struct object *right)
{
    int leftSize = SIZE(left);
    int rightSize = SIZE(right);
    int minSize = leftSize < rightSize ? leftSize : rightSize;
    int i;

    for (i = 0; i < minSize; i++) {
        if (bytePtr(left)[i] != bytePtr(right)[i]) {
            return bytePtr(left)[i] - bytePtr(right)[i];
        }
    }
    return leftSize - rightSize;
}

/*
    Main bytecode execution function
*/
int
execute(struct object *aProcess, int ticks)
{
    byte low, high, opcode;
    uint stackTop, bytePointer;
    struct object *context, *method, *arguments, *temporaries,
        *instanceVariables,
        *literals, *stack, *returnedValue = nilObject,
        *messageSelector, *receiverClass;
    unsigned char *bp;

    /* Save the process on the root stack */
    rootStack[rootTop++] = aProcess;

    /* Get current context information */
    context = aProcess->data[contextInProcess];
    if (context == NULL || IS_SMALLINT(context)) {
        fprintf(stderr, "Error: Invalid context object\n");
        return ReturnError;
    }

    method = context->data[methodInContext];
    if (method == NULL || IS_SMALLINT(method)) {
        fprintf(stderr, "Error: Invalid method object\n");
        return ReturnError;
    }

    /* Load byte pointer */
    struct object *bytecodeObj = method->data[byteCodesInMethod];
    if (bytecodeObj == NULL || IS_SMALLINT(bytecodeObj) ||
        !(bytecodeObj->size & FLAG_BIN)) {
        fprintf(stderr, "Error: Invalid bytecode object\n");
        return ReturnError;
    }
    /* Load byte pointer */
    bp = (unsigned char *)bytePtr(bytecodeObj);

    bytePointer = integerValue(context->data[bytePointerInContext]);

    /* Load stack */
    stack = context->data[stackInContext];
    stackTop = integerValue(context->data[stackTopInContext]);

    /* No need to initialize these yet */
    temporaries = instanceVariables = arguments = literals = 0;

    for (;;) {
        /* Check if ticks have expired */
        if (ticks && (--ticks == 0)) {
            aProcess = rootStack[--rootTop];
            aProcess->data[contextInProcess] = context;
            aProcess->data[resultInProcess] = returnedValue;
            context->data[bytePointerInContext] = newInteger(bytePointer);
            context->data[stackTopInContext] = newInteger(stackTop);
            return(ReturnTimeExpired);
        }

        /* Fetch opcode */
        opcode = bp[bytePointer++];
        low = opcode & 0x0F;
        high = opcode >> 4;

        /* Handle extended opcode case */
        if (high == Extended) {
            high = low;
            low = bp[bytePointer++];
        }

        /* Dispatch based on bytecode */
        switch (high) {
            case PushConstant:
                if (debugging) {
                    printf("PushConstant %d\n", low);
                }

                switch(low) {
                    case 0: case 1: case 2: case 3: case 4:
                    case 5: case 6: case 7: case 8: case 9:
                        stack->data[stackTop++] = newInteger(low);
                        break;
                    case nilConst:
                        stack->data[stackTop++] = nilObject;
                        break;
                    case trueConst:
                        stack->data[stackTop++] = trueObject;
                        break;
                    case falseConst:
                        stack->data[stackTop++] = falseObject;
                        break;
                    default:
                        fprintf(stderr, "Unknown push constant: %d\n", low);
                        return ReturnError;
                }
                break;

            case PushLiteral:
                if (debugging) {
                    printf("PushLiteral %d\n", low);
                }
                if (!literals) {
                    literals = method->data[literalsInMethod];
                }
                stack->data[stackTop++] = literals->data[low];
                break;

            case SendMessage: {
                int cacheIndex;

                if (debugging) {
                    printf("SendMessage\n");
                }

                if (!literals) {
                    literals = method->data[literalsInMethod];
                }
                messageSelector = literals->data[low];
                arguments = stack->data[--stackTop];

                /* Find method from message symbol */
                receiverClass = CLASS(arguments->data[receiverInArguments]);

                /* Check method cache */
                cacheIndex = ((uint)messageSelector + (uint)receiverClass) % cacheSize;
                if ((cache[cacheIndex].name == messageSelector) &&
                    (cache[cacheIndex].class == receiverClass)) {
                    method = cache[cacheIndex].method;
                    cacheHit++;
                } else {
                    /* Cache miss - do method lookup */
                    cacheMiss++;
                    method = lookupMethod(messageSelector, receiverClass);
                    if (!method) {
                        /* Method not found - return does-not-understand error */
                        aProcess = rootStack[--rootTop];
                        aProcess->data[contextInProcess] = context;
                        aProcess->data[resultInProcess] = messageSelector;
                        return ReturnBadMethod;
                    }

                    /* Update cache */
                    cache[cacheIndex].name = messageSelector;
                    cache[cacheIndex].class = receiverClass;
                    cache[cacheIndex].method = method;
                }

                /* Build a new context for the method */
                rootStack[rootTop++] = arguments;
                rootStack[rootTop++] = method;
                rootStack[rootTop++] = context;

                /* Save current context information */
                context->data[stackTopInContext] = newInteger(stackTop);
                context->data[bytePointerInContext] = newInteger(bytePointer);

                /* Create the new context */
                context = gcalloc(contextSize);
                context->class = ContextClass;
                context->data[previousContextInContext] = rootStack[--rootTop];
                method = context->data[methodInContext] = rootStack[--rootTop];
                arguments = context->data[argumentsInContext] = rootStack[--rootTop];

                /* Allocate temporaries and stack for new context */
                temporaries = context->data[temporariesInContext] =
                    gcalloc(integerValue(method->data[temporarySizeInMethod]));
                stack = context->data[stackInContext] =
                    gcalloc(integerValue(method->data[stackSizeInMethod]));

                /* Initialize new context */
                context->data[stackTopInContext] = newInteger(0);
                stackTop = 0;
                literals = instanceVariables = 0;
                context->data[bytePointerInContext] = newInteger(0);
                bytePointer = 0;
                bp = (unsigned char *)bytePtr(method->data[byteCodesInMethod]);
                break;
            }

            case DoSpecial:
                if (debugging) {
                    printf("DoSpecial %d\n", low);
                }

                switch (low) {
                    case SelfReturn:
                        /* Return 'self' (receiver) */
                        if (!arguments) {
                            arguments = context->data[argumentsInContext];
                        }
                        returnedValue = arguments->data[receiverInArguments];
                        goto doReturn;

                    case StackReturn:
                        /* Return top of stack */
                        returnedValue = stack->data[--stackTop];

                    doReturn:
                        /* Return to previous context */
                        context = context->data[previousContextInContext];

                        /* If no previous context, we're done */
                        if ((context == 0) || (context == nilObject)) {
                            aProcess = rootStack[--rootTop];
                            aProcess->data[contextInProcess] = context;
                            aProcess->data[resultInProcess] = returnedValue;
                            return ReturnReturned;
                        }

                        /* Restore state from previous context */
                        arguments = instanceVariables = literals = temporaries = 0;
                        stack = context->data[stackInContext];
                        stackTop = integerValue(context->data[stackTopInContext]);
                        stack->data[stackTop++] = returnedValue;
                        method = context->data[methodInContext];
                        bp = (unsigned char *)bytePtr(method->data[byteCodesInMethod]);
                        bytePointer = integerValue(context->data[bytePointerInContext]);
                        break;

                    case PopTop:
                        /* Discard top stack value */
                        stackTop--;
                        break;

                    default:
                        fprintf(stderr, "Unknown DoSpecial: %d\n", low);
                        return ReturnError;
                }
                break;

            default:
                /* Unimplemented bytecode */
                fprintf(stderr, "Unimplemented bytecode: %d\n", high);
                return ReturnError;
        }
    }
}
