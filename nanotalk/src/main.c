/*
    main.c - Simple REPL for NanoTalk

    Provides a minimal Read-Eval-Print Loop for the Agon Light port
    of Little Smalltalk
*/

#include "simple_memory.h"
#include "simple_object.h"
#include "simple_interp.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_BUFFER_SIZE 256
#define PROMPT "NanoTalk> "
#define VERSION "0.1"

/* Global variables to track initialization state */
static int systemInitialized = 0;
static int gcCount = 0;


/* Forward declarations */
static void initSystem(void);
static struct object* compileExpression(const char* expression);
static void evaluateAndPrint(const char* expression);
static void showHelp(void);
static char* readLine(void);
static struct object* createBasicMethod(const char* source);

/*
    Main entry point for NanoTalk REPL
*/
int main(int argc, char** argv) {
    char* input;
    int quit = 0;

    printf("NanoTalk v%s for Agon Light\n", VERSION);
    printf("Type 'help' for commands, 'quit' to exit\n\n");

    /* Initialize the Smalltalk system */
    initSystem();

    /* Main REPL loop */
    while (!quit) {
        printf(PROMPT);

        /* Read a line of input */
        input = readLine();

        if (input == NULL) {
            /* End of file reached */
            quit = 1;
            continue;
        }

        /* Strip leading/trailing whitespace */
        char* cmd = input;
        while (*cmd == ' ' || *cmd == '\t') cmd++;

        /* Handle special commands */
        if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "exit") == 0) {
            quit = 1;
        } else if (strcmp(cmd, "help") == 0) {
            showHelp();
        } else if (strcmp(cmd, "gc") == 0) {
            printf("Forcing garbage collection...\n");
            gcollect(0);
            printf("GC complete. Total collections: %d\n", gcCount);
        } else if (strlen(cmd) > 0) {
            /* Evaluate the expression and print the result */
            evaluateAndPrint(cmd);
        }

        free(input);
    }

    printf("Goodbye!\n");
    return 0;
}

/*
    Initialize the NanoTalk system
*/
static void initSystem(void) {
    if (systemInitialized) return;

    /* Initialize memory system */
    printf("Initializing NanoTalk system...\n");
    memoryInit(1000, 2000);  /* Configure memory sizes as appropriate for Agon */

    /* Create basic objects */
    nilObject = staticAllocate(0);
    nilObject->class = nilObject;  /* Circular reference until proper classes exist */

    trueObject = staticAllocate(0);
    falseObject = staticAllocate(0);

    /* Create base classes */
    SmallIntClass = newClass("SmallInt", nilObject, 0);
    StringClass = newClass("String", nilObject, 0);
    ArrayClass = newClass("Array", nilObject, 0);
    BlockClass = newClass("Block", nilObject, 0);
    ContextClass = newClass("Context", nilObject, 0);

    /* Create a method dictionary for SmallInt with basic arithmetic */
    struct object* dict = SmallIntClass->data[methodsInClass];
    struct object* plus = newSymbol("+", 1);
    struct object* minus = newSymbol("-", 1);

    /* Create and add addition method */
    struct object* addMethod = createBasicMethod("+ arg ^ self primAdd: arg");

    /* Add to dictionary - would normally use dictionary methods */
    struct object* keys = dict->data[keysInDict];
    struct object* values = dict->data[valuesInDict];

    /* For demo purposes, manually add to dictionary */
    // if (SIZE(keys) == 0) {
    //     struct object* newKeys = newArray(1);
    //     struct object* newValues = newArray(1);
    //     newKeys->data[0] = plus;
    //     newValues->data[0] = addMethod;
    //     dict->data[keysInDict] = newKeys;
    //     dict->data[valuesInDict] = newValues;
    // }

    /* Initialize primitive method handlers */
    /* (This would be more developed in a full implementation) */

    systemInitialized = 1;
    printf("System initialized.\n");
}

/*
    Read a line of input from the user
*/
static char* readLine(void) {
    char buffer[INPUT_BUFFER_SIZE];

    if (fgets(buffer, INPUT_BUFFER_SIZE, stdin) == NULL) {
        return NULL;  /* EOF */
    }

    /* Remove trailing newline */
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len-1] == '\n') {
        buffer[len-1] = '\0';
    }

    return strdup(buffer);
}

/*
    Compile an expression into a method

    For a simple expression like "1 + 2", this would create a method:
    ^ 1 + 2
*/
static struct object* compileExpression(const char* expression) {
    /* In a full implementation, this would parse and compile the expression */
    /* For this minimal version, we'll create a method with hardcoded bytecodes */

    if (strncmp(expression, "1 + 2", 5) == 0) {
        /* Create a method that pushes 1, 2, sends + message, and returns */
        static byte bytecodes[] = {
            /* Push constant 1 */
            PushConstant * 16 + 1,

            /* Push constant 2 */
            PushConstant * 16 + 2,

            /* Send "+" message */
            SendMessage * 16 + 0,

            /* Return the result */
            DoSpecial * 16 + StackReturn
        };

        /* Create method object */
        struct object* method = newMethod(newSymbol("doIt", 4), 0, 3);

        /* Create bytecodes */
        struct object* bytecodeObj = gcialloc(sizeof(bytecodes));
        memcpy(bytePtr(bytecodeObj), bytecodes, sizeof(bytecodes));
        method->data[byteCodesInMethod] = bytecodeObj;

        /* Create literals array with "+" symbol */
        struct object* literals = newArray(1);
        literals->data[0] = newSymbol("+", 1);
        method->data[literalsInMethod] = literals;

        return method;
    } else {
        /* A real implementation would parse and compile various expressions */
        printf("Sorry, only '1 + 2' is supported in this demo version.\n");
        return NULL;
    }
}

/*
    Create a basic method from source code
    This is a placeholder for a real compiler
*/
static struct object* createBasicMethod(const char* source) {
    /* Very simple placeholder - would normally use a real compiler */
    /* Just create a method that adds two small integers */

    static byte addBytecodes[] = {
        /* Push argument (the right side of +) */
        PushArgument * 16 + 1,

        /* Push receiver (self - left side of +) */
        PushArgument * 16 + 0,

        /* Do primitive addition */
        DoPrimitive * 16 + 1,
        0,  /* argument count */
        10, /* primitive number for + */

        /* Return the result */
        DoSpecial * 16 + StackReturn
    };

    /* Create a method */
    struct object* selector = newSymbol("+", 1);
    struct object* method = newMethod(selector, 0, 3);

    /* Add bytecodes */
    struct object* bytecodeObj = gcialloc(sizeof(addBytecodes));
    memcpy(bytePtr(bytecodeObj), addBytecodes, sizeof(addBytecodes));
    method->data[byteCodesInMethod] = bytecodeObj;

    /* Need empty literals array */
    method->data[literalsInMethod] = newArray(0);

    return method;
}

/*
    Evaluate an expression and print the result
*/
static void evaluateAndPrint(const char* expression) {
    struct object* method = compileExpression(expression);

    if (method == NULL) {
        return;  /* Compilation failed */
    }

    /* Create a context and process for execution */
    struct object* arguments = newArray(1);
    arguments->data[0] = nilObject;  /* 'self' for the doIt method */

    struct object* context = newContext(method, arguments);

    struct object* process = staticAllocate(3);
    process->data[contextInProcess] = context;

    /* Execute the method */
    rootStack[rootTop++] = process;

    int returnCode = execute(process, 1000);  /* Allow up to 1000 bytecodes */

    process = rootStack[--rootTop];
    struct object* result = process->data[resultInProcess];

    /* Print the result */
    if (returnCode == ReturnReturned) {
        if (IS_SMALLINT(result)) {
            printf("=> %d\n", integerValue(result));
        } else if (result == nilObject) {
            printf("=> nil\n");
        } else if (result == trueObject) {
            printf("=> true\n");
        } else if (result == falseObject) {
            printf("=> false\n");
        } else {
            printf("=> an object of class %s\n",
                bytePtr(result->class->data[nameInClass]));
        }
    } else {
        printf("Error executing expression (code %d)\n", returnCode);
    }
}

/*
    Display help information
*/
static void showHelp(void) {
    printf("\nNanoTalk Help:\n");
    printf("  expressions - Type Smalltalk expressions to evaluate them\n");
    printf("  help        - Display this help\n");
    printf("  gc          - Force garbage collection\n");
    printf("  quit        - Exit NanoTalk\n\n");
    printf("Currently only the expression '1 + 2' is supported in this demo version.\n");
    printf("A full implementation would support a wide range of Smalltalk expressions.\n\n");
}
