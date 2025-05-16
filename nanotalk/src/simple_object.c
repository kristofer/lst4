/*
    simple_object.c - Basic Smalltalk object implementation
*/

#include "simple_object.h"
#include <string.h>


/* Symbol comparison function */
int
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

/* Create a new Symbol */
struct object *
newSymbol(const char *name, int length)
{
    struct object *symbol;

    if (length < 0) {
        length = strlen(name);
    }

    symbol = gcialloc(length);
    symbol->class = SmallIntClass; /* Placeholder until SymbolClass exists */

    memcpy(bytePtr(symbol), name, length);

    return symbol;
}

/* Create a new String */
struct object *
newString(const char *text, int length)
{
    struct object *string;

    if (length < 0) {
        length = strlen(text);
    }

    string = gcialloc(length);
    string->class = StringClass;

    memcpy(bytePtr(string), text, length);

    return string;
}

/* Create a new Class */
struct object *
newClass(const char *name, struct object *superClass, int instanceSize)
{
    struct object *newClass;

    newClass = staticAllocate(ClassSize);
    newClass->data[nameInClass] = newSymbol(name, -1);
    newClass->data[parentClassInClass] = superClass;
    newClass->data[methodsInClass] = newDictionary();
    newClass->data[instanceSizeInClass] = newInteger(instanceSize);

    return newClass;
}

/* Create a new Method */
struct object *
newMethod(struct object *selector, int tempSize, int stackSize)
{
    struct object *method;

    method = gcalloc(methodSize);
    method->class = nilObject; /* Placeholder until MethodClass exists */

    method->data[nameInMethod] = selector;
    method->data[stackSizeInMethod] = newInteger(stackSize);
    method->data[temporarySizeInMethod] = newInteger(tempSize);

    return method;
}

/* Create a new Context for method execution */
struct object *
newContext(struct object *method, struct object *arguments)
{
    struct object *context;
    int stackSize, tempSize;

    /* Create context object */
    context = gcalloc(contextSize);
    context->class = ContextClass;

    /* Set method */
    context->data[methodInContext] = method;
    context->data[argumentsInContext] = arguments;

    /* Create stack */
    stackSize = integerValue(method->data[stackSizeInMethod]);
    context->data[stackInContext] = newArray(stackSize);

    /* Create temporaries */
    tempSize = integerValue(method->data[temporarySizeInMethod]);
    if (tempSize > 0) {
        context->data[temporariesInContext] = newArray(tempSize);
    } else {
        context->data[temporariesInContext] = nilObject;
    }

    /* Initialize byte pointer and stack top */
    context->data[bytePointerInContext] = newInteger(0);
    context->data[stackTopInContext] = newInteger(0);

    return context;
}

/* Create a new Array */
struct object *
newArray(int size)
{
    struct object *array;
    int i;

    array = gcalloc(size);
    array->class = ArrayClass;

    /* Initialize with nil */
    for (i = 0; i < size; i++) {
        array->data[i] = nilObject;
    }

    return array;
}

/* Create a new Dictionary */
struct object *
newDictionary(void)
{
    struct object *dict;

    dict = gcalloc(2);
    dict->class = nilObject; /* Placeholder until DictionaryClass exists */

    /* Empty keys and values arrays */
    dict->data[keysInDict] = newArray(0);
    dict->data[valuesInDict] = newArray(0);

    return dict;
}

/* Method lookup */
struct object *
lookupMethod(struct object *selector, struct object *class)
{
    struct object *dict, *keys, *values;
    int i, size;

    /* Traverse class hierarchy */
    while (class != nilObject) {
        dict = class->data[methodsInClass];
        keys = dict->data[keysInDict];
        values = dict->data[valuesInDict];
        size = SIZE(keys);

        /* Linear search through methods */
        for (i = 0; i < size; i++) {
            if (selector == keys->data[i]) {
                return values->data[i];
            }
        }

        /* Try superclass */
        class = class->data[parentClassInClass];
    }

    /* Method not found */
    return NULL;
}
