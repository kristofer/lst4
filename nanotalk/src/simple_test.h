/*
    simple_test.c - Basic test framework for Agon Smalltalk

    Tests the core functionality of our simplified Smalltalk system
*/

#include "simple_memory.h"
#include "simple_object.h"
#include "simple_interp.h"
#include <stdio.h>
#include <string.h>

/* Test data */
static byte testBytecodes[] = {
    /* Push constant 1 */
    PushConstant * 16 + 1,

    /* Push constant 2 */
    PushConstant * 16 + 2,

    /* Send "+" message */
    SendMessage * 16 + 0,  /* 0 is the index in literals */

    /* Return the result */
    DoSpecial * 16 + StackReturn
};

/* Test if we have a SmallInt with expected value */
int testSmallInt(struct object *obj, int expected) {
    if (!IS_SMALLINT(obj)) {
        printf("FAIL: Expected SmallInt, got something else\n");
        return 0;
    }

    int value = integerValue(obj);
    if (value != expected) {
        printf("FAIL: Expected %d, got %d\n", expected, value);
        return 0;
    }

    printf("PASS: Got SmallInt value %d\n", value);
    return 1;
}

/* Create a simple addition method */
struct object *createAddMethod(void) {
    struct object *method, *bytecodes, *literals, *plusSymbol;

    /* Create "+" symbol */
    plusSymbol = newSymbol("+", 1);

    /* Create method */
    method = newMethod(plusSymbol, 0, 3);

    /* Create bytecodes */
    bytecodes = gcialloc(sizeof(testBytecodes));
    memcpy(bytePtr(bytecodes), testBytecodes, sizeof(testBytecodes));
    method->data[byteCodesInMethod] = bytecodes;

    /* Create literals array with "+" symbol */
    literals = newArray(1);
    literals->data[0] = plusSymbol;
    method->data[literalsInMethod] = literals;

    return method;
}

/* Set up and run a test method */
int testMethod(struct object *method) {
    struct object *process, *context, *stack, *result;

    /* Create execution context */
    context = newContext(method, nilObject);

    /* Create a process */
    process = staticAllocate(3);
    process->data[contextInProcess] = context;

    /* Execute the method */
    printf("Executing test method...\n");
    rootStack[rootTop++] = process;

    int returnCode = execute(process, 100); /* Allow up to 100 bytecodes */

    process = rootStack[--rootTop];
    result = process->data[resultInProcess];

    printf("Execution complete, return code: %d\n", returnCode);

    if (returnCode != ReturnReturned) {
        printf("FAIL: Method did not complete normally\n");
        return 0;
    }

    /* Check the result of 1+2 */
    return testSmallInt(result, 3);
}

/* Main test function */
int main(void) {
    /* Initialize memory system */
    printf("Initializing memory system...\n");
    memoryInit(1000, 2000);

    /* Initialize object system */
    printf("Creating basic objects...\n");

    /* Create nil, true, false */
    nilObject = staticAllocate(0);
    trueObject = staticAllocate(0);
    falseObject = staticAllocate(0);

    /* Create basic classes */
    printf("Creating basic classes...\n");
    SmallIntClass = newClass("SmallInt", nilObject, 0);
    StringClass = newClass("String", nilObject, 0);
    ArrayClass = newClass("Array", nilObject, 0);
    BlockClass = newClass("Block", nilObject, 0);
    ContextClass = newClass("Context", nilObject, 0);

    /* Run tests */
    printf("Setting up test method...\n");
    struct object *method = createAddMethod();

    printf("Running test...\n");
    int success = testMethod(method);

    printf("\nTest %s\n", success ? "PASSED" : "FAILED");
    //printf("GC count: %d\n", gcCount);

    return success ? 0 : 1;
}
