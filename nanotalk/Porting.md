# Porting Little Smalltalk to Agon Light

## User


<context>
The following items were attached by the user. They are up-to-date and don't need to be re-read.

<directories>
```lst4/src/ImageBuilder/Makefile
CC=gcc
CFLAGS=-Wall -g -I../source

image: imageBuilder imageSource
	./imageBuilder <imageSource

imageBuilder: imageBuilder.o
	rm -f imageBuilder
	$(CC) -o imageBuilder imageBuilder.o

jconv.class: jconv.java
	javac jconv.java

binaryImage:
	java jconv <image

# always clean up after yourself
clean:
	rm -f imageBuilder.o imageBuilder image

mv:
	mv binaryImage ~/Java

```

```c lst4/src/ImageBuilder/imageBuilder.c
/*
	image building utility
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "memory.h"
#include "interp.h"

FILE *fin;
char inputBuffer[1500], *p, tokenBuffer[80];

struct object *lookupGlobal(char * name, int ok_missing);
int parseStatement(void), parseExpression(void), parseTerm(void);
struct object *newOrderedArray(void), *newArray(int size);

void
sysError(char * a, char * b)
{
	printf("in syserror %s %s\n", a, b);
	exit(1);
}

/*
	The following are roots for the file out
*/

struct object *nilObject, *trueObject, *falseObject,
	*globalValues, *SmallIntClass, *ArrayClass, *BlockClass,
	*IntegerClass;

struct object *SymbolClass, *CharClass;

#ifdef gcalloc
# undef gcalloc
#endif

struct object *
gcalloc(int size)
{
	struct object * result;

	result = 
		malloc(sizeof(struct object) + size * sizeof(struct object *));
	if (result == 0) {
		sysError("out of memory", "gcalloc");
	}
	SETSIZE(result, size);
	while (size > 0) {
		result->data[--size] = nilObject;
	}
	return result;
}

struct byteObject *
binaryAlloc(int size)
{
	int osize;
	struct byteObject * result;

	osize = (size + BytesPerWord - 1) / BytesPerWord;
	result = (struct byteObject *) gcalloc(osize);
	SETSIZE(result, size);
	result->size |= FLAG_BIN;
	return result;
}

/* ------------------------------------------------------------- */
/*	Errors   */
/* ------------------------------------------------------------- */

int
parseError(char * msg)
{
	char * q;

	for ( q = inputBuffer; q != p; )
		printf("%c", *q++);
	printf("\n%s\n", msg);
	while (*q)
		printf("%c", *q++);
	printf("\n");
	exit(1);
	return 0;
}

/* ------------------------------------------------------------- */
/*	names   */
/* ------------------------------------------------------------- */

int globalTop = 0;
char *globalNames[100];
struct object *globals[100];

void
addGlobalName(char * name, struct object * value)
{
	char * newName;

	newName = strdup(name);
	if (!newName) {
		sysError("out of memory", "newname in add global");
	}
	globalNames[globalTop] = newName;
	globals[globalTop] = value;
	globalTop++;
}

struct object *
lookupGlobal(char *name, int ok_missing)
{
	int i;

	for (i = 0; i < globalTop; i++) {
		if (strcmp(name, globalNames[i]) == 0) {
			return globals[i];
		}
	}
	/* not found, return 0 */
	if (!ok_missing) {
		sysError("Missing global", name);
	}
	return 0;
}

/* ------------------------------------------------------------- */
/*	Lexical Analysis  */
/* ------------------------------------------------------------- */

void
inputMethodText()
{
	char c;

	p = inputBuffer;
	while (1) {
		while ((c = fgetc(fin)) != '\n')
			*p++ = c;
		*p++ = '\n';
		if ((c = fgetc(fin)) == '!') {
			if ((c = fgetc(fin)) == '\n') {
				*p = '\0';
				return;
				}
			*p++ = '!';
			*p++ = c;
			}
		else
			*p++ = c;
		}
}

void
skipSpaces()
{
	while ((*p == ' ') || (*p == '\t') || (*p == '\n')) p++;
	if (*p == '\"') {
		p++;
		while (*p && (*p != '\"')) p++;
		if (*p != '\"') parseError("unterminated comment");
		p++; skipSpaces();
		}
}

int
isDigit(char p)
{
	if ((p >= '0') && (p <= '9')) return 1;
	return 0;
}

int
isIdentifierChar(char p)
{
	if ((p >= 'a') && (p <= 'z')) return 1;
	if ((p >= 'A') && (p <= 'Z')) return 1;
	return 0;
}

int
isBinary(char p)
{
	switch(p) {
		case '+': case '*': case '-': case '/': case '<': case '=':
		case '>': case '@': case '~': case ',':
			return 1;
		}
	return 0;
}

void
readBinary()
{
	tokenBuffer[0] = *p++;
	if (isBinary(*p)) {
		tokenBuffer[1] = *p++;
		tokenBuffer[2] = '\0';
		}
	else
		tokenBuffer[1] = '\0';
	skipSpaces();
}

int
readIdentifier()
{
	int keyflag;
	char *q = tokenBuffer;
	while (isIdentifierChar(*p) || isDigit(*p)) *q++ = *p++;
	*q = '\0';
	if (*p == ':') {	/* It's a keyword identifier */
		keyflag = 1;
		*q++ = ':';
		*q = '\0';
		p++;
		}
	else
		keyflag = 0;
	skipSpaces();
	return keyflag;
}

int
readInteger()
{
	int val, neg = 0;

	if (*p == '-') {
		neg = 1;
		++p;
	}
	val = *p++ - '0';
	while (isDigit(*p)) {
		val = 10 * val + (*p++ - '0');
	}
	skipSpaces();
	return neg ? -val : val;

}

/* ------------------------------------------------------------- */
/*	new instances of standard things   */
/* ------------------------------------------------------------- */

int symbolTop = 0;
struct object *oldSymbols[500];

int
symbolBareCmp(char * left, int leftsize, char * right, int rightsize)
{
	int minsize = leftsize;
	int i;

	if (rightsize < minsize) minsize = rightsize;
	for (i = 0; i < minsize; i++) {
		if (left[i] != right[i]) {
			if (left[i] < right[i]) {
			    return -1;
			} else {
			    return 1;
			}
		}
	}
	return leftsize - rightsize;
}

int
symbolCmp(struct object * left, struct object * right)
{
	return symbolBareCmp(bytePtr(left), SIZE(left),
		bytePtr(right), SIZE(right));
}

struct object *
newSymbol(char * text)
{
	int i;
	struct byteObject *result;

		/* first see if it is already a symbol */
	for (i = 0; i < symbolTop; i++) {
		if (symbolBareCmp(text, strlen(text),
			bytePtr(oldSymbols[i]), SIZE(oldSymbols[i])) == 0) {
			return oldSymbols[i];
		}
	}

		/* not there, make a new one */
	result = binaryAlloc(strlen(text));
	for (i = 0; i < strlen(text); i++) {
		result->bytes[i] = text[i];
	}
	result->class = lookupGlobal("Symbol", 0);
	oldSymbols[symbolTop++] = (struct object *)result;
	return (struct object *) result;
}

struct object *
newClass(char * name)
{
	struct object *newC;

	newC = gcalloc(ClassSize);
	newC->data[nameInClass] = newSymbol(name);
	return newC;
}

struct object *
newNode(struct object *v, struct object *l, struct object *r)
{
	struct object * result;

	result = gcalloc(3);
	result->class = lookupGlobal("Node", 0);
	result->data[0] = v;
	result->data[1] = l;
	result->data[2] = r;
	return result;
}

struct object *
newTree(void)
{
	struct object * result;

	result = gcalloc(1);
	result->class = lookupGlobal("Tree", 0);
	return result;
}

struct object *
newDictionary(void)
{
	struct object *result;

	result = gcalloc(2);
	result->class = lookupGlobal("Dictionary", 0);
	result->data[0] = newOrderedArray();
	result->data[1] = newArray(0);
	return result;
}

/* ------------------------------------------------------------- */
/*	Code Generation   */
/* ------------------------------------------------------------- */

# define ByteBufferTop 512
unsigned char byteBuffer[ByteBufferTop];
unsigned byteTop;

void
genByte(int v)
{
	byteBuffer[byteTop++] = v;
	if (byteTop >= ByteBufferTop) {
		sysError("too many bytecodes", "");
	}
}

void
genVal(int v)
{
	if ((v < 0) || (v > 0xFFFF)) {
		sysError("illegal value", "");
	}
	genByte(v & 0xFF);
	genByte(v >> 8);
}

void
genValPos(int pos, int v)
{
	if ((v < 0) || (v > 0xFFFF)) {
		sysError("illegal value", "");
	}
	byteBuffer[pos] = v & 0xFF;
	byteBuffer[pos+1] = v >> 8;
}

void
genInstruction(int a, int b)
{
	/*printf("gen instruction %d %d\n", a, b);*/
	if (b < 16) {
		genByte(a * 16 + b);
	} else {
		genInstruction(0, a);
		genByte(b);
	}
}

struct object *
buildByteArray()
{
	struct byteObject * newObj;
	int i;

	newObj = binaryAlloc(byteTop);
	for (i = 0; i < byteTop; i++)
		newObj->bytes[i] = byteBuffer[i];
	newObj->class = lookupGlobal("ByteArray", 0);
	return (struct object *) newObj;
}

# define LiteralBufferTop 60
struct object * litBuffer[LiteralBufferTop];
unsigned litTop = 0;

int
addLiteral(struct object *a)
{
	litBuffer[litTop++] = a;
	if (litTop >= LiteralBufferTop) {
		sysError("too many literals", "");
	}
	return litTop-1;
}

struct object *
buildLiteralArray(void)
{
	int i;
	struct object * result;

	if (litTop == 0)
		return nilObject;
	result = gcalloc(litTop);
	result->class = lookupGlobal("Array", 0);
	for (i = 0; i < litTop; i++)
		result->data[i] = litBuffer[i];
	return result;
}

# define ArgumentBufferTop 30
char * argumentNames[ArgumentBufferTop];
int argumentTop;

void
addArgument(char *name)
{
	char *p;

	p = strdup(name);
	if (!p) {
		sysError("malloc failure", "addArguments");
	}
	argumentNames[argumentTop++] = p;
}

# define TempBufferTop 500
char *tempBuffer[TempBufferTop];
int tempTop, maxTemp;

void
addTemporary(char *name)
{
	char *p;

	p = strdup(name);
	if (!p) {
		sysError("malloc failure", "addTemporary");
	}
	tempBuffer[tempTop++] = p;
	if (tempTop > maxTemp) {
		maxTemp = tempTop;
	}
}

struct object * currentClass;

/* ------------------------------------------------------------- */
/*	big bang   */
/* ------------------------------------------------------------- */

void
bigBang(void)
{
	struct object *ObjectClass, *MetaObjectClass, *ClassClass,
		*NilClass, *TrueClass, *FalseClass, *StringClass,
		*TreeClass, *DictionaryClass, *OrderedArrayClass,
		*MetaClassClass, *ByteArrayClass;

	/*
	 * First, make the nil (undefined) object;
	 * notice its class is wrong
	 */
	nilObject = gcalloc(0);

	/*
	 * Second, make class for Symbol;
	 * this will allow newClass to work correctly
	 */
	SymbolClass = gcalloc(ClassSize + 1);
	addGlobalName("Symbol", SymbolClass);
	SymbolClass->data[nameInClass] = newSymbol("Symbol");

	/* now we can fix up nil's class */
	NilClass = newClass("Undefined");
	addGlobalName("Undefined", NilClass);
	nilObject->class = NilClass;
	addGlobalName("nil", nilObject);

	/* make up the object / metaobject mess */
	ObjectClass = newClass("Object");
	addGlobalName("Object", ObjectClass);
	MetaObjectClass = newClass("MetaObject");
	addGlobalName("MetaObject", MetaObjectClass);
	ObjectClass->class = MetaObjectClass;
	ObjectClass->data[parentClassInClass] = nilObject;

	/* And the Class/MetaClass mess */
	ClassClass = newClass("Class");
	addGlobalName("Class", ClassClass);
	MetaClassClass = newClass("MetaClass");
	addGlobalName("MetaClass", MetaClassClass);
	ClassClass->class = MetaClassClass;

	/* now make up a bunch of other classes */
	BlockClass = newClass("Block");
	addGlobalName("Block", BlockClass);

	/* create Char with extra slot for instance var */
	CharClass = gcalloc(ClassSize + 1);
	addGlobalName("Char", CharClass);
	CharClass->data[nameInClass] = newSymbol("Char");

	/* SmallInt has an extra class variable, just like Symbol and Char */
	SmallIntClass = gcalloc(ClassSize + 1);
	addGlobalName("SmallInt", SmallIntClass);
	SmallIntClass->data[nameInClass] = newSymbol("SmallInt");

	IntegerClass = newClass("Integer");
	addGlobalName("Integer", IntegerClass);

	TrueClass = newClass("True");
	addGlobalName("True", TrueClass);
	trueObject = gcalloc(0);
	trueObject->class = TrueClass;
	addGlobalName("true", trueObject);

	FalseClass = newClass("False");
	addGlobalName("False", FalseClass);
	falseObject = gcalloc(0);
	falseObject->class = FalseClass;
	addGlobalName("false", falseObject);

	ArrayClass = newClass("Array");
	addGlobalName("Array", ArrayClass);
	ByteArrayClass = newClass("ByteArray");
	addGlobalName("ByteArray", ByteArrayClass);

	OrderedArrayClass = newClass("OrderedArray");
	addGlobalName("OrderedArray", OrderedArrayClass);

	StringClass = newClass("String");
	addGlobalName("String", StringClass);

	TreeClass = newClass("Tree");
	addGlobalName("Tree", TreeClass);

	DictionaryClass = newClass("Dictionary");
	addGlobalName("Dictionary", DictionaryClass);

	/* finally, we can fill in the fields in class Object */
	ObjectClass->data[methodsInClass] = newDictionary();
	ObjectClass->data[instanceSizeInClass] = newInteger(0);
	ClassClass->data[instanceSizeInClass] = newInteger(0);

	/* can make global name, but can't fill it in */
	globalValues = gcalloc(2);
	addGlobalName("globals", globalValues);
}

/* ------------------------------------------------------------- */
/*	Parsing */
/* ------------------------------------------------------------- */

int
parseInteger(void)
{
	int i;

	i = readInteger();
	if ((i >= 0) && (i < 10)) {
		genInstruction(PushConstant, i);
	} else {
		genInstruction(PushLiteral, addLiteral(newInteger(i)));
	}
	return 1;
}

int
parsePrimitive(void)
{
	int primitiveNumber, argumentCount;

	/* skip over the left bracket */
	p++; skipSpaces();

	/* then read the primitive number */
	if (isDigit(*p))
		primitiveNumber = readInteger();
	else
		return parseError("missing primitive number");

	/* then read the arguments */
	for (argumentCount = 0; *p && (*p != '>'); argumentCount++)
		if (!parseTerm()) {
			return 0;
		}

	/* make sure we ended correctly */
	if (*p == '>') {
		p++; skipSpaces();
		}
	else
		return parseError("missing > at end of primitive");

	/* generate instructions */
	genInstruction(DoPrimitive, argumentCount);
	genByte(primitiveNumber);

	/* Success */
	return(1);
}

struct object *
newString(char * text)
{
	int size, i;
	struct byteObject *newObj;

	size = strlen(text);
	newObj = binaryAlloc(size);
	for (i = 0; i < size; i++)
		newObj->bytes[i] = text[i];
	newObj->class = lookupGlobal("String", 0);
	return (struct object *) newObj;
}

int
parseString(void)
{
	char *q;

	p++;
	for (q = tokenBuffer; *p && *p != '\''; )
		*q++ = *p++;
	if (*p != '\'')
		return parseError("missing end of string");
	p++; skipSpaces(); *q = '\0';
	genInstruction(PushLiteral, addLiteral(newString(tokenBuffer)));
	return 1;
}

int
lookupInstance(struct object *class, char *text, int *low)
{
	struct object *var;
	int size, i;


 	/* first check superclasses */
 	var = class->data[parentClassInClass];
 	if (var && var != nilObject) {
 		size = lookupInstance(var, text, low);
 		if (size >= 0) return size;
	} else {	/* no superclass */
 		*low = 0;
	}
 
	/* Check our own list of variables */
	var = class->data[variablesInClass];
	if (var && var != nilObject) {
		size = SIZE(var);
	} else {
		size = 0;
	}
	for (i = 0; i < size; i++) {
		if (symbolBareCmp(text, strlen(text),
		 bytePtr(var->data[i]), (SIZE(var->data[i]))) == 0) {
			return(*low);
		}
		*low += 1;
	}
	return(-1);
}

int superMessage = 0;

char *lowConstants[4] = {"nil", "true", "false", 0};

int
nameTerm(char *name)
{
	int i;

	/* see if temporary */
	for (i = 0; i < tempTop; i++) {
		if (strcmp(name, tempBuffer[i]) == 0) {
			genInstruction(PushTemporary, i);
			return 1;
		}
	}

	/* see if argument */
	for (i = 0; i < argumentTop; i++) {
		if (strcmp(name, argumentNames[i]) == 0) {
			genInstruction(PushArgument, i);
			return 1;
		}
	}

	/* see if super */
	if (strcmp(name, "super") == 0) {
		genInstruction(PushArgument, 0);
		superMessage = 1;
		return 1;
	}

	/* see if low constant */
	for (i = 0; lowConstants[i]; i++) {
		if (strcmp(lowConstants[i], name) == 0) {
			genInstruction(PushConstant, 10+i);
			return 1;
		}
	}

	/* see if instance variable */
	if (currentClass) {
		int low;

		i = lookupInstance(currentClass, name, &low);
		if (i >= 0) {
			genInstruction(PushInstance, i);
			return 1;
		}
	}

	/* see if global */
	{
		struct object *glob = lookupGlobal(name, 1);

		if (glob) {
			genInstruction(PushLiteral, addLiteral(glob));
			return 1;
		}
	}

	return(parseError("unknown identifier"));
}

int returnOp;
char * blockbackup;

int
parseBlock(void)
{
	int savedLocation, saveTop, argCount;
	char * savestart;

	savestart = p;
	p++; skipSpaces();
	genInstruction(PushBlock, tempTop);
	savedLocation = byteTop;
	genVal(0);

	saveTop = tempTop; argCount = 0;
	if (*p == ':') {
		while(1) {
			p++; skipSpaces();
			if (! isIdentifierChar(*p))
				return parseError("missing identifier");
			if (readIdentifier())
				return parseError("keyword illegal");
			addTemporary(tokenBuffer); argCount++;
			if (*p == '|') break;
			if (*p != ':') return parseError("missing colon:");
		}
		p++; skipSpaces();
	}
	if (*p == ']') {
		genInstruction(PushConstant, nilConst);
	} else {
		int saveReturnOp = returnOp;

		returnOp = BlockReturn;
		while (1) {
			if (! parseStatement()) {
				parseError("Statement syntax inside block");
			}
			if (*p == '.') {
				p++;
				skipSpaces();
			}
			if (*p == ']') {
				break;
			} else {
				genInstruction(DoSpecial, PopTop);
			}
		}
		returnOp = saveReturnOp;
	}
	p++; skipSpaces();	/* skip over ] */
	genInstruction(DoSpecial, StackReturn);
	genValPos(savedLocation, byteTop);
	tempTop = saveTop;

	/* set blockbackup to textual start of block */
	blockbackup = savestart;
	return 1;
}

int
parseSymbol(void)
{
	char *q;

	p++;
	for (q=tokenBuffer; isIdentifierChar(*p) || (*p == ':'); )
		*q++ = *p++;
	*q = '\0'; skipSpaces();
	genInstruction(PushLiteral, addLiteral(newSymbol(tokenBuffer)));
	return 1;
}

int
parseChar(void)
{
	struct object * newObj;

	p++;
	newObj = gcalloc(1);
	newObj->class = lookupGlobal("Char", 0);
	newObj->data[0] = newInteger((int) *p);
	genInstruction(PushLiteral, addLiteral(newObj));
	p++; skipSpaces();
	return 1;
}

int
parseTerm(void)
{
	/* make it so anything other than a block zeros out backup var */
	blockbackup = 0;
	superMessage = 0;

	if (*p == '(') {
		p++; skipSpaces();
		if (! parseExpression()) return 0;
		if (*p != ')')
			return parseError("unbalanced parenthesis");
		p++; skipSpaces();
		return 1;
		}
	if (*p == '<') return parsePrimitive();
	if (*p == '$') return parseChar();
	if (isDigit(*p) || (*p == '-')) return parseInteger();
	if (*p == '\'') return parseString();
	if (isIdentifierChar(*p)) {
		readIdentifier();
		return nameTerm(tokenBuffer);
		}
	if (*p == '[') return parseBlock();
	if (*p == '#') return parseSymbol();
	return parseError("illegal start of expression");
}

char *unaryBuiltIns[] = {"isNil", "notNil", 0};
char *binaryBuiltIns[] = {"<", "<=", "+", 0};

int
parseUnaryContinuation(void)
{
	int litNumber, done;
	char *q;

	while (isIdentifierChar(*p)) {
		q = p;
		if (readIdentifier()) {
			p = q;	/* oops, was a keyword */
			break;
			}
		done = 0;
		if (! superMessage) {	int i;
			for (i = 0; unaryBuiltIns[i]; i++)
				if (strcmp(tokenBuffer, unaryBuiltIns[i]) == 0) {
					genInstruction(SendUnary, i);
					done = 1;
					}
			}
		if (! done) {
			genInstruction(MarkArguments, 1);
			litNumber = addLiteral(newSymbol(tokenBuffer));
/*printf("unary %s\n", tokenBuffer);*/
			if (superMessage) {
				genInstruction(DoSpecial, SendToSuper);
				genByte(litNumber);
				}
			else
				genInstruction(SendMessage, litNumber);
			superMessage = 0;
			}
		}
	return 1;
}

int
parseBinaryContinuation(void)
{
	int messLiteral, i, done;
	char messbuffer[80];

	if (! parseUnaryContinuation()) return 0;
	while (isBinary(*p)) {
		readBinary();
/*printf("binary symbol %s\n", tokenBuffer);*/
		strcpy(messbuffer, tokenBuffer);
		if (! parseTerm()) return 0;
		if (! parseUnaryContinuation()) return 0;

		done = 0;
		if (! superMessage)
			for (i = 0; binaryBuiltIns[i]; i++)
				if (strcmp(messbuffer, binaryBuiltIns[i]) == 0) {
					genInstruction(SendBinary, i);
					done = 1;
					}

		if (! done) {
			messLiteral = addLiteral(newSymbol(messbuffer));
			genInstruction(MarkArguments, 2);
			if (superMessage) {
				genInstruction(DoSpecial, SendToSuper);
				genByte(messLiteral);
				superMessage = 0;
				}
			else
				genInstruction(SendMessage, messLiteral);
			}
		}
	return 1;
}

int
optimizeBlock(void)
{
	if (*p != '[') {
		if (! parseTerm()) return 0;
		parseError("missing block as optimized block argument");
	} else {
		p++; skipSpaces();
		if (*p == ']') {
			genInstruction(PushConstant, 0);
			p++; skipSpaces();
			return 1;
			}
		while(1) {
			if (! parseStatement()) return 0;
			if (*p == '.') p++, skipSpaces();
			if (*p == ']') break;
			genInstruction(DoSpecial, PopTop);
			}
		p++; skipSpaces();
		/* just leave last expression on stack */
		}
	return 1;
}

int
controlFlow(int opt1, char * rest, int opt2)
{
	int save1, save2;
	char *q;

	genInstruction(DoSpecial, opt1);
	save1 = byteTop;
	genVal(0);
	if (!optimizeBlock()) {
		parseError("syntax error in control flow");
	}
	genInstruction(DoSpecial, Branch);
	save2 = byteTop;
	genVal(0);
	genValPos(save1, byteTop);
	q = p;
	if (isIdentifierChar(*p) && readIdentifier() && (strcmp(tokenBuffer, rest) == 0)) {
		if (!optimizeBlock()) {
			parseError("syntax error in control cascade");
		}
	} else {
		p = q;
		genInstruction(PushConstant, opt2);
	}
	genValPos(save2, byteTop);
	return 1;
}

int
optimizeLoop(int branchInstruction)
{
	int L1, L2;

	/* back up to start of block and try again */
	p = blockbackup;
	L1 = byteTop;
	optimizeBlock();
	genInstruction(DoSpecial, branchInstruction);
	L2 = byteTop;
	genVal(0);
	if (!(isIdentifierChar(*p) && readIdentifier()))
		return parseError("can't get message again in optimized block");
	/* now read the body */
	optimizeBlock();
	genInstruction(DoSpecial, PopTop);
	genInstruction(DoSpecial, Branch);
	genVal(L1);
	genValPos(L2, byteTop);
	genInstruction(PushConstant, 0);
	return 1;
}

int
parseKeywordContinuation(void)
{
	int argCount, i, done, saveSuper;
	char messageBuffer[100];

	saveSuper = superMessage;
	if (! parseBinaryContinuation()) return 0;
	strcpy(messageBuffer,"");
	argCount = 0;
	if (isIdentifierChar(*p) && readIdentifier()) {
		if (strcmp(tokenBuffer, "ifTrue:") == 0)
			return controlFlow(BranchIfFalse, "ifFalse:", nilConst);
		else if (strcmp(tokenBuffer, "ifFalse:") == 0)
			return controlFlow(BranchIfTrue, "ifTrue:", nilConst);
		else if (strcmp(tokenBuffer, "and:") == 0)
			return controlFlow(BranchIfFalse, "", falseConst);
		else if (strcmp(tokenBuffer, "or:") == 0)
			return controlFlow(BranchIfTrue, "", trueConst);
		else if ((strcmp(tokenBuffer, "whileTrue:") == 0) &&
				blockbackup)
			return optimizeLoop(BranchIfFalse);
		else if ((strcmp(tokenBuffer, "whileFalse:") == 0) &&
				blockbackup)
			return optimizeLoop(BranchIfTrue);
		else
		do{
			strcat(messageBuffer, tokenBuffer);
			argCount++;
			if (! parseTerm()) return 0;
			if (! parseBinaryContinuation()) return 0;
			} while (isIdentifierChar(*p) && readIdentifier());
		}
	if (argCount > 0) {
/*printf("keywork message %s\n", messageBuffer);*/
		done = 0;
		if (! saveSuper)
			for (i = 0; binaryBuiltIns[i]; i++)
				if (strcmp(messageBuffer, binaryBuiltIns[i]) == 0) {
					genInstruction(SendBinary, i);
					done = 1;
					}
		if (! done) {
			genInstruction(MarkArguments, argCount+1);
			if (saveSuper) {
				genInstruction(DoSpecial, SendToSuper);
				genByte(addLiteral(newSymbol(messageBuffer)));
				superMessage = 0;
				}
			else
				genInstruction(SendMessage, addLiteral(newSymbol(messageBuffer)));
			}
		}
	return 1;
}

int
doAssignment(char * name)
{
	int i;

	for (i = 0; i < tempTop; i++)
		if (strcmp(name, tempBuffer[i]) == 0) {
			genInstruction(AssignTemporary, i);
			return 1;
			}

	if (currentClass) {int low;
		i = lookupInstance(currentClass, name, &low);
		if (i >= 0) {
			genInstruction(AssignInstance, i);
			return 1;
			}
		}

	return parseError("unknown target of assignment");
}

int
parseExpression(void)
{
	char nameBuffer[60];

	if (isIdentifierChar(*p)) {
		readIdentifier();
		if ((*p == '<') && (*(p+1) == '-')) {
			p++; p++; skipSpaces();
			strcpy(nameBuffer, tokenBuffer);
			if (! parseExpression()) return 0;
			return doAssignment(nameBuffer);
			}
		if (! nameTerm(tokenBuffer)) return 0;
	} else {
		if (! parseTerm()) {
			return 0;
		}
	}
	if (!parseKeywordContinuation()) {
		return 0;
	}
	while (*p == ';') {
		p++; skipSpaces();
		genInstruction(DoSpecial, Duplicate);
		if (! parseKeywordContinuation()) return 0;
	}
	return 1;
}

int
parseStatement(void)
{
	if (*p == '^') {	/* return statement */
		p++; skipSpaces();
		if (! parseExpression()) return 0;
		genInstruction(DoSpecial, returnOp);
		return 1;
		}
	/* otherwise just an expression */
	if (! parseExpression()) return 0;
	return 1;
}

int
parseBody(void)
{
	returnOp = StackReturn;
	while (*p) {
		if (! parseStatement()) return 0;
		genInstruction(DoSpecial, PopTop);
		if (*p == '.') {
			p++;
			skipSpaces();
			}
		}
	genInstruction(DoSpecial, SelfReturn);
	return 1;
}

int
parseMethodHeader(struct object * theMethod)
{
	char messageBuffer[100], *q;
	int keyflag;

	if (isIdentifierChar(*p)) {
		if (readIdentifier()) {					/* keyword message */
			strcpy(messageBuffer,"");
			keyflag = 1;
			while (keyflag) {
				strcat(messageBuffer,tokenBuffer);
				if (isIdentifierChar(*p) && ! readIdentifier())
					addArgument(tokenBuffer);
				else
					return parseError("missing argument after keyword");
				q = p;
				if (isIdentifierChar(*p) && readIdentifier())
					keyflag = 1;
				else {
					p = q;
					keyflag = 0;
					}
				}
			}
		else
			strcpy(messageBuffer, tokenBuffer);	/* unary message */
		}
	else if (isBinary(*p)) {					/* binary message */
		readBinary();
		strcpy(messageBuffer, tokenBuffer);
		if (! isIdentifierChar(*p))
			return parseError("missing argument");
		readIdentifier();
		addArgument(tokenBuffer);
		}
	else return parseError("ill formed method header");
	theMethod->data[nameInMethod] = newSymbol(messageBuffer);
	printf("Method %s\n", messageBuffer);
	return 1;
}

int
parseTemporaries(void)
{
	tempTop = 0; maxTemp = 0;
	if (*p != '|') return 1;
	p++; skipSpaces();
	while (*p != '|') {
		if (! isIdentifierChar(*p)) return parseError("need identifier");
		if (readIdentifier()) return parseError("keyword illegal");
		addTemporary(tokenBuffer);
		}
	p++; skipSpaces();
	return 1;
}

int
parseMethod(struct object * theMethod)
{
	if (! parseMethodHeader(theMethod))
		return 0;
	if (! parseTemporaries())
		return 0;
	if (parseBody()) {
		theMethod->data[literalsInMethod] = buildLiteralArray();
		theMethod->data[byteCodesInMethod] = buildByteArray();
		theMethod->data[stackSizeInMethod] = newInteger(19);
		theMethod->data[temporarySizeInMethod] = newInteger(maxTemp);
		theMethod->data[classInMethod] = currentClass;
		theMethod->data[textInMethod] = newString(inputBuffer);
		return 1;
	}
	return 0;
}


/* ------------------------------------------------------------- */
/*	Input Processing   */
/* ------------------------------------------------------------- */

/*	read the expression beyond the begin statement */
struct object *
BeginCommand(void)
{
	struct object * bootMethod;

	byteTop = 0;
	litTop = 0;
	argumentTop = 0;
	currentClass = 0;
	tempTop = 0; maxTemp = 0;

	if (parseBody()) {
	printf("parsed begin command ok\n");
		bootMethod = gcalloc(methodSize);
		bootMethod->class = lookupGlobal("Method", 0);
		bootMethod->data[nameInMethod] = newSymbol("boot");
		bootMethod->data[literalsInMethod] = buildLiteralArray();
		bootMethod->data[byteCodesInMethod] = buildByteArray();
		bootMethod->data[stackSizeInMethod] = newInteger(12);
	} else {
		parseError("building begin method");
	}

	return bootMethod;
}

/*
 * insert()
 *	Insert an element in the array at the given position
 *
 * Creates a new Array-ish object of the same class as "array",
 * and returns it filled in as requested.
 */
struct object *
insert(struct object *array, int index, struct object *val)
{
	int i, j;
	struct object *o;

	/*
	 * Clone the current object, including class.  Make one
	 * extra slot in the Array storage.
	 */
	o = gcalloc(SIZE(array) + 1);
	o->class = array->class;

	/*
	 * Copy up to the index
	 */
	for (i = 0; i < index; ++i) {
		o->data[i] = array->data[i];
	}

	/*
	 * Put in the new element at this position
	 */
	j = i;
	o->data[i++] = val;

	/*
	 * Now copy the rest
	 */
	for (; j < SIZE(array); ++j) {
		o->data[i++] = array->data[j];
	}
	return(o);
}

/*
 * dictionaryInsert()
 *	Insert a key/value pair into the Dictionary
 */
void
dictionaryInsert(struct object *dict, struct object *index,
		struct object *value)
{
	struct object *keys = dict->data[0], *vals = dict->data[1];
	int i, lim, res;

	/*
	 * Scan the OrderedArray "keys" to find where we fit in
	 */
	for (i = 0, lim = SIZE(keys); i < lim; ++i) {
		res = symbolCmp(index, keys->data[i]);

		/*
		 * We should go in before this node
		 */
		if (res < 0) {
			dict->data[0] = insert(keys, i, index);
			dict->data[1] = insert(vals, i, value);
			return;
		} else if (res > 0) {
			continue;
		} else {
			sysError("dictionary insert:", "duplicate key");
		}
	}

	/*
	 * The new element goes at the end
	 */
	dict->data[0] = insert(keys, i, index);
	dict->data[1] = insert(vals, i, value);
}

/*
 * newArray()
 *	Allocate a new array
 *
 * All slots are initialized to nil
 */
struct object *
newArray(int size)
{
	struct object *result;
	int i;

	result = gcalloc(size);
	result->class = lookupGlobal("Array", 0);
	for (i = 0; i < size; ++i) {
		result->data[i] = nilObject;
	}
	return(result);
}

/*
 * newOrderedArray()
 *	Return a new, empty ordered array
 */
struct object *
newOrderedArray(void)
{
	struct object *result;

	result = gcalloc(0);
	result->class = lookupGlobal("OrderedArray", 0);
	return(result);
}

void
MethodCommand(void)
{
	struct object *theMethod;

	/* read class name */
	readIdentifier();
	currentClass = lookupGlobal(tokenBuffer, 1);
	if (! currentClass) {
		sysError("unknown class in Method", tokenBuffer);
	}

	inputMethodText();

	p = inputBuffer; skipSpaces();

	theMethod = gcalloc(methodSize);
	theMethod->class = lookupGlobal("Method", 0);

	/* fill in method class */
	byteTop = 0;
	litTop = 0;
	argumentTop = 1;

	/*
	 * If successful compile, insert into the method dictionary
	 */
	if (parseMethod(theMethod)) {
		dictionaryInsert(currentClass->data[methodsInClass],
			theMethod->data[nameInMethod], theMethod);
	}
}

void
RawClassCommand(void)
{
	struct object *nClass, *supClass, *instClass;
	int instsize;

	/* read the class */
	readIdentifier();
	nClass = lookupGlobal(tokenBuffer, 1);
	printf("Class %s\n", tokenBuffer);
	if (!nClass) {
		nClass = newClass(tokenBuffer);
		nClass->data[nameInClass] = newSymbol(tokenBuffer);
		addGlobalName(tokenBuffer, nClass);
	}

	/* now read the instance class */
	readIdentifier();
	instClass = lookupGlobal(tokenBuffer, 1);
	if (! instClass) {
		sysError("can't find instance class", tokenBuffer);
	}
	nClass->class = instClass;

	/* now read the super class */
	readIdentifier();
	supClass = lookupGlobal(tokenBuffer, 1);
	if (! supClass)  {
		sysError("can't find super class", tokenBuffer);
	}
	nClass->data[parentClassInClass] = supClass;

	/* rest are instance variables */
	litTop = 0;

	/* Now parse the new instance variables */
	while (*p) {
		if (!isIdentifierChar(*p)) {
			sysError("looking for var", p);
		}
		readIdentifier();
		addLiteral(newSymbol(tokenBuffer));
	}

	/* That's the total of our instance variables */
	instsize = litTop;

	/* Add on size of superclass space */
 	if (supClass != nilObject) {
 		instsize +=
 			integerValue(supClass->data[instanceSizeInClass]);
 	}

	nClass->data[instanceSizeInClass] = newInteger(instsize);
	nClass->data[variablesInClass] = buildLiteralArray();
			/* make a tree for new methods */
	nClass->data[methodsInClass] = newDictionary();
}

/*
 * ClassCommand()
 *	Build the base and meta classes automatically
 *
 * Doesn't support class variables, but handles most of imageSource
 * cases.
 */
void
ClassCommand(void)
{
	char *class, *super, *ivars;

	/* Read the class and superclass */
	readIdentifier();
	class = strdup(tokenBuffer);
	readIdentifier();
	super = strdup(tokenBuffer);

	/* Stash away the instance variable string */
	skipSpaces();
	ivars = strdup(p);

	/* Build the metaclass */
	sprintf(inputBuffer, "RAWCLASS Meta%s Class Meta%s", class, super);
	p = inputBuffer + 9;
	RawClassCommand();

	/* Now the instance class */
	sprintf(inputBuffer, "RAWCLASS %s Meta%s %s %s", class, class,
		super, ivars);
	p = inputBuffer + 9;
	RawClassCommand();
	free(class); free(super); free(ivars);
}

/* ------------------------------------------------------------- */
/*	writing image   */
/* ------------------------------------------------------------- */

# define imageMaxNumberOfObjects 5000
struct object * writtenObjects[imageMaxNumberOfObjects];
int imageTop = 0;

void
writeWord(int i, FILE * fp)
{
	if (i < 0) {
		sysError("writeWord: negative value", (void *)i);
	}
	if (i >= 255) {
		fputc(255, fp);
		writeWord(i - 255, fp);
	} else {
		fputc(i, fp);
	}
}

void
imageOut(FILE *fp, struct object *obj)
{
	int i, size;

	if (imageTop > imageMaxNumberOfObjects) {
		fprintf(stderr,"too many indirect objects\n");
		exit(1);
	}

	/*printf("writing out object %d\n", obj);*/

	if (obj == 0) {
		sysError("writing out null object", 0);
		return;
	}

	/* Integer objects are simply encoded as the binary value */
	if (IS_SMALLINT(obj)) {
		int val = integerValue(obj);

		writeWord(2, fp);
		fwrite(&val, sizeof(val), 1, fp);
		return;
	}

	/* see if already written */
	for (i = 0; i < imageTop; i++) {
		if (obj == writtenObjects[i]) {
			if (i == 0)
				writeWord(5, fp);
			else {
				writeWord(4, fp);
				writeWord(i, fp);
			}
			return;
		}
	}

	/* not written, do it now */
	writtenObjects[imageTop++] = obj;

	if (obj->size & FLAG_BIN) {	/* byte objects */
		struct byteObject * bobj = (struct byteObject *) obj;
		size = SIZE(obj);
		writeWord(3, fp);
		writeWord(size, fp);
		/*fprintf(fp, "3 %d \n", size);*/
		for (i = 0; i < size; i++)
			/*fprintf(fp," %d ", bobj->bytes[i]);*/
			writeWord(bobj->bytes[i], fp);
		/*fprintf(fp,"\n");*/
		if (obj->class == 0) {
			printf("byte object 0x%x has null class\n",
				(unsigned int)obj);
		}
		imageOut(fp, obj->class);
		return;
	}

	/* ordinary objects */
	size = SIZE(obj);
	/*fprintf(fp,"1 %d ", size);*/
	writeWord(1, fp);
	writeWord(size, fp);
	if (obj->class == 0) {
		printf("object 0x%x has null class\n", (unsigned int)obj);
	}
	imageOut(fp, obj->class);
	for (i = 0; i < size; i++) {
		imageOut(fp, obj->data[i]);
	}
	/*fprintf(fp,"\n");*/
}

/* ------------------------------------------------------------- */
/*	fix up symbol tables   */
/* ------------------------------------------------------------- */

struct object *
symbolTreeInsert(struct object * base, struct object * symNode)
{
	if (base == nilObject)
		return symNode;
	if (symbolCmp(symNode->data[valueInNode], base->data[valueInNode])  < 0)
		base->data[leftInNode] =
			symbolTreeInsert(base->data[leftInNode], symNode);
	else
		base->data[rightInNode] =
			symbolTreeInsert(base->data[rightInNode], symNode);
	return base;
}

struct object *
fixSymbols(void)
{
	struct object * t;
	int i;

	t = newTree();
	for (i = 0; i < symbolTop; i++)
		t->data[0] = symbolTreeInsert(t->data[0],
			newNode(oldSymbols[i], nilObject, nilObject));
	return t;
}

void
fixGlobals(void)
{
	struct object *t;
	int i;

	t = globalValues;
	t->class = lookupGlobal("Dictionary", 0);
	t->data[0] = newOrderedArray();
	t->data[1] = newArray(0);

	/*
	 * Insert each class name as a reference to the class
	 * object itself.
	 */
	for (i = 0; i < globalTop; i++) {
		if (strncmp(globalNames[i], "Meta", 4) == 0) {
			continue;
		}
		dictionaryInsert(t, newSymbol(globalNames[i]),
			globals[i]);
	}

	/*
	 * Insert this object itself under the name "Smalltalk"
	 */
	dictionaryInsert(t, newSymbol("Smalltalk"), t);
}

/* ------------------------------------------------------------- */
/*	checkGlobals   */
/* ------------------------------------------------------------- */
void
checkGlobals(void)
{
	int i;
	struct object *o;

	for (i = 0; i < globalTop; i++) {
		o = globals[i];
		if (!o->class) {
			sysError("Never defined", globalNames[i]);
		}
	}
}

/* ------------------------------------------------------------- */
/*	main program   */
/* ------------------------------------------------------------- */

int
main(void)
{
	FILE *fd;
	struct object *bootMethod = 0;

	/* big bang -- create the first classes */
	bigBang();
	addArgument("self");

	if ((fin = fopen("imageSource", "r")) == NULL)
		sysError("file in error", "imageSource");

	/* then read the image source file */
	while(fgets((char *) inputBuffer, 1000, fin)) {
		p = inputBuffer; skipSpaces();
		readIdentifier();

		if (strcmp(tokenBuffer, "BEGIN") == 0) {
			bootMethod = BeginCommand();
		} else if (strcmp(tokenBuffer, "RAWCLASS") == 0) {
			RawClassCommand();
		} else if (strcmp(tokenBuffer, "CLASS") == 0) {
			ClassCommand();
		} else if (strcmp(tokenBuffer, "COMMENT") == 0) {
			/* nothing */ ;
		} else if (strcmp(tokenBuffer, "METHOD") == 0) {
			MethodCommand();
		} else if (strcmp(tokenBuffer, "END") == 0) {
			break;
		} else {
			sysError("unknown command ", tokenBuffer);
		}
	}

	fclose(fin);

	/* then create the tree of symbols */
	SymbolClass->data[symbolsInSymbol] = fixSymbols();
	fixGlobals();

	/* see if anything was never defined in the class source */
	checkGlobals();

	if ((fd = fopen("image", "w")) == NULL) {
		sysError("file out error", "image");
	}
	printf("starting to file out\n");
	imageOut(fd, nilObject);
	imageOut(fd, trueObject);
	imageOut(fd, falseObject);
	imageOut(fd, globalValues);
	imageOut(fd, SmallIntClass);
	imageOut(fd, IntegerClass);
	imageOut(fd, ArrayClass);
	imageOut(fd, BlockClass);
	imageOut(fd, lookupGlobal("Context", 0));
	imageOut(fd, bootMethod);
	imageOut(fd, newSymbol("<"));
	imageOut(fd, newSymbol("<="));
	imageOut(fd, newSymbol("+"));
	imageOut(fd, newSymbol("doesNotUnderstand:"));
	fclose(fd);
	printf("%d objects written\n", imageTop);
	printf("bye for now!\n");
	return(0);
}

```

```lst4/src/ImageBuilder/imageSource
COMMENT
COMMENT		Source for the initial image
COMMENT
COMMENT		must be compiled by the image builder
COMMENT	
COMMENT-----------------------------------------------------------
COMMENT RAWCLASS name   instanceOf subclassOf  (instance variable names)
COMMENT  or... CLASS name subclassOf (instance vars)
COMMENT RAWCLASS doesn't automatically build Meta class; CLASS does
COMMENT-----------------------------------------------------------
COMMENT		The strange circular world at the beginning
COMMENT RAWCLASS Object MetaObject nil
RAWCLASS Class      MetaClass Object      name parentClass methods size variables
RAWCLASS MetaObject Class     Class
RAWCLASS MetaClass  Class     MetaObject
COMMENT--------------------------------------------------------
COMMENT-------then, the remaining classes in the initial image
COMMENT----------------------------
CLASS Undefined     Object
CLASS Boolean       Object
CLASS True          Boolean
CLASS False         Boolean
CLASS Process       Object context state result
CLASS Context       Object method arguments temporaries stack bytePointer stackTop previousContext
CLASS Block         Context argumentLocation creatingContext oldBytePointer
CLASS Method        Object name byteCodes literals stackSize temporarySize class text
CLASS Magnitude     Object
RAWCLASS MetaSymbol    Class         MetaMagnitude     symbols
RAWCLASS Symbol        MetaSymbol    Magnitude
RAWCLASS MetaChar      Class         MetaMagnitude     chars
RAWCLASS Char          MetaChar      Magnitude         value
CLASS Collection    Magnitude
CLASS List          Collection         elements
CLASS Dictionary    Collection        keys values
CLASS Array         Collection
CLASS OrderedArray  Array
CLASS ByteArray     Array
CLASS MetaString    Class         MetaArray
CLASS String        Array
CLASS Set	    Collection		members growth
CLASS IdentitySet   Set
CLASS Number        Magnitude
CLASS Integer       Number
CLASS SmallInt      Number
RAWCLASS MetaSmallInt Class           MetaNumber           seed
RAWCLASS SmallInt   MetaSmallInt Number
CLASS Link          Object            value next
CLASS Node          Object            value left right
CLASS Interval      Collection        low high step
CLASS File          Object            fileID
CLASS Association	Magnitude	key value
CLASS Tree		Collection	root
COMMENT ---------- Classes having to do with parsing ------------
CLASS Parser Object text index tokenType token argNames tempNames instNames maxTemps errBlock lineNum
CLASS ParserNode Object lineNum
CLASS Encoder Object name byteCodes index literals stackSize maxStack
CLASS BodyNode ParserNode statements
CLASS ReturnNode ParserNode expression
CLASS AssignNode ParserNode target expression
CLASS LiteralNode ParserNode value
CLASS ArgumentNode ParserNode position
CLASS TemporaryNode ParserNode position
CLASS InstNode ParserNode position
CLASS PrimitiveNode ParserNode number arguments
CLASS BlockNode ParserNode statements temporaryLocation
CLASS CascadeNode ParserNode head list
CLASS MessageNode ParserNode receiver name arguments
COMMENT ---------- method bodies ------------
COMMENT -----------Object-------------------
METHOD MetaObject
in: object at: index put: value
	" change data field in object, used during initialization "
	" returns the intialized object "
	<5 value object index>
!
METHOD Object
in: object at: index
	" browse instance variable via debugger "
	<24 object index>.
	self primitiveFailed
!
METHOD Object
isNil
	^ false
!
METHOD Object
notNil
	^ true
!
METHOD Object
== arg
	<1 self arg>
!
METHOD Object
= arg
	^ self == arg
!
METHOD Object
~= arg
	^ (self = arg) not
!
METHOD Object
~~ arg
	^ (self == arg) not
!
METHOD Object
class
	<2 self>
!
METHOD Object
printString
	^ self class printString
!
METHOD Object
isMemberOf: aClass
	^ self class == aClass
!
METHOD Object
isKindOf: aClass	| clas |
	clas <- self class.
	[ clas notNil ] whileTrue:
		[ clas == aClass ifTrue: [ ^ true ].
		  clas <- clas superclass ].
	^ false
!
METHOD Object
respondsTo: aMessage
	^ self class allMethods includes: aMessage
!
METHOD Object
print
	self printString do: [ :c | c print ]
!
METHOD Object
printNl
	self print. Char newline print
!
METHOD Object
question: text	| answer |
	text print.
	answer <- String input.
	(answer notNil)
		ifTrue: [ answer <- answer at: 1 ifAbsent: [ $n ] ].
	^ answer = $y or: [ answer = $Y]
!
METHOD Object
error: str
		" print the message "
	str printNl.
		" then halt "
	<19>
!
METHOD Object
debug
	<18>
!
METHOD Object
primitiveFailed
	self error: 'Primitive failed'
!
METHOD Object
species
	" By default, we re-instantiate ourselves as our own Class "
	^ self class
!
METHOD Object
hash
	" Most objects should generate something based on their value "
	^ self class printString hash
!
METHOD Object
become: other
	" Exchange identity with another object "
	(Array with: self) elementsExchangeIdentityWith: (Array with: other)
!
METHOD Object
doesNotUnderstand: aSel
	self error: (self printString + ' (class '+
		(self class printString) +
		'): does not understand ' + aSel printString)
!
METHOD Object
basicSize
	<4 self>.
	self primitiveFailed
!
METHOD Object
subclassResponsibility
	self error: 'Subclass responsibility'
!
METHOD Object
basicDo: aBlock
	^ self do: aBlock
!
COMMENT ---------Class-------------------
METHOD Class
name: n parent: c variables: v
	" create a new class with the given characteristics "
	name <- n.
	parentClass <- c.
	methods <- Dictionary new.
	size <- v size + c size.
	variables <- v
!
METHOD Class
size
	^ size
!
METHOD Class
superclass
	^ parentClass
!
METHOD Class
variables
	^ variables
!
METHOD Class
subclasses
	self subclasses: 0
!
METHOD Class
subclasses: indent
	globals do: [ :obj |
		((obj isKindOf: Class) and: [ obj superclass == self])
			ifTrue: [
				1 to: indent do: [:ignore| $  print ].
				obj printNl.
				obj subclasses: indent + 4 ] ]
!
METHOD Class
listMethods
	methods keysDo:
		[ :name | name printNl ]
!
METHOD Class
allMethods | allMethods |
	parentClass isNil
		ifTrue: [ allMethods <- Dictionary new ]
		ifFalse: [ allMethods <- parentClass allMethods ].
	methods binaryDo: [ :n :m | allMethods at: n put: m ].
	^ allMethods
!
METHOD Class
listAllMethods
	self allMethods keysDo: [:n| n printNl ]
!
METHOD Class
viewMethod: nm  | meth |
	meth <- self allMethods at: nm
		ifAbsent: [ ^ self error: 'no such method'].
	meth text print.
	^ ''
!
METHOD Class
editMethod: nm	| meth text |
	meth <- methods at: nm
		ifAbsent: [ ^ self error: 'no such method'].
	text <- meth text edit.
	(self question: 'compile method?')
		ifTrue: [ self addMethod: text ]
!
METHOD Class
subclass: nm
	^ self subclass: nm variables: (Array new: 0)
		classVariables: (Array new: 0)
!
METHOD Class
subclass: nm variables: v
	^ self subclass: nm variables: v classVariables: (Array new: 0)
!
METHOD Class
subclass: nm variables: v classVariables: cv | meta |
	meta <- Class new name: ('Meta' + nm asString) asSymbol
		parent: self class
		variables: cv.
	globals at: nm put: ( meta new name: nm
		parent: self
		variables: v ).
	^ 'subclass created: ' + nm printString
!
METHOD Class
addMethod	| text |
	text <- ' ' edit.
	(self question: 'compile method?')
		ifTrue: [ self addMethod: text ]
!
METHOD Class
addMethod: text | meth |
	meth <- self parseMethod: text.
	meth notNil ifTrue: [
		methods at: meth name put: meth.
		Method flushCache.
		'ok' printNl.
		^ meth
	].
	^ nil
!
METHOD Class
removeMethod: methName
	methods removeKey: methName ifAbsent: [
		self error: 'Method not present: ' + (methName printString)
	].
	Method flushCache.
!
METHOD Class
view: methodName
		" print the text of the given method "
	(methods at: methodName
		ifAbsent: [ ^ self error: 'no such method'])
			text print
!
METHOD Class
parseMethod: text
	^ (Parser new
		text: text instanceVars: self instanceVariables) parse: self
!
METHOD Class
new
	" return a new instance of ourselves "
	<7 self size>
!
METHOD Class
basicNew
	" Like new "
	<7 self size>
!
METHOD Class
instanceVariables	| names |
		" return all our variable names "
	parentClass notNil
		ifTrue: [ names <- parentClass instanceVariables ]
		ifFalse: [ names <- Array new: 0 ].
	(variables isNil or: [ variables isEmpty ])
		ifFalse: [ names <- names + variables ].
	^ names
!
METHOD Class
methods
	" return the tree of methods "
	^ methods
!
METHOD Class
printString
	" just return our name "
	^ name printString
!
METHOD Class
isMeta
	^ (name asString from: 1 to: 4) = 'Meta'
!
METHOD Class
fileOut: f | tag |
	self isMeta ifTrue: [
		tag <- '=' + (name asString from: 5 to: 99)
	] ifFalse: [
		tag <- '!' + name asString
	].
	tag <- tag + String newline.
	methods do: [:m|
		f write: tag.
		f write: (m text printString).
		f write: ('!' + String newline)
	]
!
METHOD Class
fileOut | f |
	f <- File openWrite: (name printString + '.st').
	f isNil ifTrue: [ self error: 'File creation failed' ].
	self isMeta ifFalse: [
		self class fileOut: f
	].
	self fileOut: f.
	f close
!
COMMENT -----------Undefined--------------
METHOD MetaUndefined
new
	" there is only one nil object "
	^ nil
!
METHOD Undefined
printString
	^ 'nil'
!
METHOD Undefined
isNil
	" yes, we are nil "
	^ true
!
METHOD Undefined
notNil
	" no, we are not not-nil "
	^ false
!
METHOD Undefined
main	| command |
	" initialize classes "
	Char initialize.

	" main execution loop "
	[ '-> ' print. command <- String input. command notNil ]
		whileTrue: [ command isEmpty
			ifFalse: [ command doIt printNl ] ]
!
COMMENT -----------Boolean--------------
METHOD Boolean
and: aBlock
	^ self
		ifTrue: [ aBlock value ]
		ifFalse: [ false ]
!
METHOD Boolean
or: aBlock
	^ self
		ifTrue: [ true ]
		ifFalse: [ aBlock value ]
!
METHOD Boolean
not
	^ self
		ifTrue: [ false ]
		ifFalse: [ true ]
!
METHOD Boolean
ifFalse: falseBlock ifTrue: trueBlock
	^ self ifTrue: [ trueBlock  value ] ifFalse: [ falseBlock value ]
!
METHOD Boolean
ifTrue: aBlock
	^ self ifTrue: [ aBlock value ] ifFalse: [ nil ]
!
METHOD Boolean
ifFalse: aBlock
	^ self ifTrue: [ nil ] ifFalse: [ aBlock value ]
!
COMMENT -----------True--------------
METHOD MetaTrue
new
	" there is only one true value "
	^ true
!
METHOD True
not
	^ false
!
METHOD True
printString
	^ 'true'
!
METHOD True
ifTrue: trueBlock ifFalse: falseBlock
	^ trueBlock value
!
METHOD True
or: aBlock
	^ true
!
METHOD True
and: aBlock
	^ aBlock value
!
COMMENT -----------False--------------
METHOD MetaFalse
new
	" there is only one false value "
	^ false
!
METHOD False
not
	^ true
!
METHOD False
printString
	^ 'false'
!
METHOD False
ifTrue: trueBlock ifFalse: falseBlock
	^ falseBlock value
!
METHOD False
or: aBlock
	^ aBlock value
!
METHOD False
and: aBlock
	^ false
!
COMMENT -----------Process--------------
METHOD Process
doExecute: ticks
	<6 self ticks>
!
METHOD Process
context: aContext
	context <- aContext
!
METHOD Process
context
	^ context
!
METHOD Process
execute | r |
	r <- self doExecute: 0.
	(r = 3) ifTrue: [
		" Note: state field is filled in with arguments on error "
		(state at: 1) print. ' (class ' print.
		(state at: 1) class print. ') ' print.
		'does not understand: ' print.  result printNl
	].
	(r = 4) ifTrue: [ ^ result ]
		ifFalse: [ 'Backtrace:' printNl.
			context backtrace. ^ nil ]
!
COMMENT -----------Context--------------
METHOD Context
setup: aMethod withArguments: a
	method <- aMethod.
	arguments <- Array new: 1.
	bytePointer <- 0.
	stack <- Array new: method stackSize.
	stackTop <- 0.
	temporaries <- Array new: method temporarySize.
!
METHOD Context
perform: aMethod withArguments: a | proc |
	self setup: aMethod withArguments: a.
	proc <- Process new.
	proc context: self.
	^ proc execute
!
METHOD Context
backtrace | narg |
		" backtrace context calls "
	narg <- 0.
	method name print.
	'(' print.
	arguments do: [:a |
		(narg > 0) ifTrue: [ ', ' print ].
		a class print.
		narg <- narg+1
	].
	')' printNl.
	previousContext notNil
		ifTrue: [ previousContext backtrace ]
!
METHOD Context
previousContext
	^ previousContext
!
COMMENT ---------- Blocks ------------
METHOD Block
argCount
	self error: 'Incorrect argument passing to Block'
!
METHOD Block
value
	" start block execution "
	<8 self>
	(self argCount)
!
METHOD Block
value: a
	" start block execution "
	<8 a self>
	(self argCount)
!
METHOD Block
value: a value: b
	" start block execution "
	<8 a b self>
	(self argCount)
!
METHOD Block
whileTrue: aBlock
	self value ifTrue: [ aBlock value. ^ self whileTrue: aBlock ]
!
METHOD Block
whileFalse: aBlock
	self value ifFalse: [ aBlock value. ^ self whileFalse: aBlock ]
!
METHOD Block
backtrace | narg |
		" backtrace context calls "
	'block from ' print. method name print.
	'(' print.
	narg <- 0.
	arguments do: [:a |
		(narg > 0) ifTrue: [', ' print ].
		a class print.
		narg <- narg+1
	].
	')' printNl.
	previousContext notNil
		ifTrue: [ previousContext backtrace ]
!
COMMENT -----------Magnitude--------------
METHOD Magnitude
<= arg
	^ self < arg or: [ self = arg ]
!
METHOD Magnitude
> arg
	^ arg < self
!
METHOD Magnitude
>= arg
	^ (self > arg) or: [ self = arg ]
!
METHOD Magnitude
min: arg
	^ self < arg ifTrue: [ self ] ifFalse: [ arg ]
!
METHOD Magnitude
max: arg
	^ self < arg ifTrue: [ arg ] ifFalse: [ self ]
!
METHOD Magnitude
between: low and: high
	^ low <= self and: [ self <= high ]
!
COMMENT -----------Symbol--------------
METHOD MetaSymbol
intern: string
	<23 string Symbol>
!
METHOD MetaSymbol
new: fromString | sym |
	^ symbols at: fromString
		ifAbsent: [ symbols add: (self intern: fromString) ]
!
METHOD Symbol
printString
	<23 self String>
!
METHOD Symbol
asString
	^self printString
!
METHOD Symbol
hash
	^self printString hash
!
METHOD Symbol
asSymbol
	^self
!
METHOD Symbol
= aString
		" works with either symbol or string arguments "
	^ self printString = aString printString
!
METHOD Symbol
< arg
		" works with either symbol or string arguments "
	^ self printString < arg printString
!
COMMENT -----------Method--------------
METHOD MetaMethod
name: n byteCodes: b literals: l stackSize: s temporarySize: ts class: c text: t
	| newMethod |
	newMethod <- self new.
	super in: newMethod at: 1 put: n.
	super in: newMethod at: 2 put: b.
	super in: newMethod at: 3 put: l.
	super in: newMethod at: 4 put: s.
	super in: newMethod at: 5 put: ts.
	super in: newMethod at: 6 put: c.
	super in: newMethod at: 7 put: t.
	^ newMethod
!
METHOD MetaMethod
flushCache
	<34>.
	self primitiveFailed
!
METHOD Method
byteCodes
	^ byteCodes
!
METHOD Method
literals
	^ literals
!
METHOD Method
text
	^ text
!
METHOD Method
name
	^ name
!
METHOD Method
stackSize
	^ stackSize
!
METHOD Method
temporarySize
	^temporarySize
!
METHOD Method
args: argNames inst: instNames temp: tempNames
	" Hook for recording symbolic debug "
!
COMMENT -----------Chars--------------
METHOD MetaChar
initialize
	chars isNil ifTrue: [
		chars <- Array new: 257.
		1 to: 257 do: [:idx|
			chars at: idx put: (Char basicNew: idx-1)
		]
	]
!
METHOD MetaChar
basicNew: value
	" create and initialize a new char "
	^ self in: self new at: 1 put: value
!
METHOD MetaChar
new: value
	" return unique Char for ASCII value (or EOF) "
	(value < 257) ifTrue: [ ^ chars at: value+1 ].

	" otherwise build a custom Char "
	^ self basicNew: value
!
METHOD MetaChar
newline
		" return newline character "
	^ self new: 10
!
METHOD MetaChar
tab
		" return tab character "
	^ self new: 9
!
METHOD MetaChar
eof
		" return an EOF indication--not a true Char, but polymorphic "
	^ self new: 256
!
METHOD MetaChar
doInput
	<9>
!
METHOD MetaChar
input	| c |
	" read a single char from input stream "
	c <- self doInput.
	(c notNil)
		ifTrue: [ ^self new: c ]
		ifFalse: [ ^nil ]
!
METHOD Char
value
		" return our ascii value as an integer "
	^ value
!
METHOD Char
hash
	^ value
!
METHOD Char
isDigit
	^ self between: $0 and: $9
!
METHOD Char
isLowerCase
	^ self between: $a and: $z
!
METHOD Char
isUpperCase
	^ self between: $A and: $Z
!
METHOD Char
isAlphabetic
	^ self isLowerCase or: [ self isUpperCase ]
!
METHOD Char
isEOF
	^ value = 256
!
METHOD Char
lowerCase
	self isUpperCase
		ifTrue: [ ^ Char new: (value - 65) + 97 ]
!
METHOD Char
upperCase
	self isLowerCase
		ifTrue: [ ^ Char new: (value - 97) + 65 ]
!
METHOD Char
isAlphanumeric
		" are we a letter or a digit? "
	^ self isAlphabetic or: [ self isDigit ]
!
METHOD Char
isBlank
		"spaces, tabs and newlines are all blank"
	^ value = 32 or: [ value = 9 or: [ value = 10 ] ]
!
METHOD Char
print
	<3 value>
!
METHOD Char
asString
	" return char as a string value "
	^ String new: 1; at: 1 put: self
!
METHOD Char
printString
	^ String new: 2; at: 1 put: $$ ; at: 2 put: self
!
METHOD Char
= aChar
	^ value = aChar value
!
METHOD Char
< aChar
	^ value < aChar value
!
COMMENT ---------- Collection ------------
METHOD Collection
noElement
	self error: 'Element not present'
!
METHOD Collection
size	| tally |
	tally <- 0.
	self do: [:i | tally <- tally + 1].
	^ tally
!
METHOD Collection
isEmpty
		" empty if there are no elements "
	^ self size = 0
!
METHOD Collection
at: value ifAbsent: exceptionBlock
	self do: [ :element | element = value ifTrue: [ ^ element ]].
	^ exceptionBlock value
!
METHOD Collection
at: value
	^ self at: value ifAbsent: [ self noElement ]
!
METHOD Collection
includes: value
	self at: value ifAbsent: [ ^ false ].
	^ true
!
METHOD Collection
asArray		| newArray index |
	newArray <- Array new: self size.
	index <- 1.
	self do: [ :element | newArray at: index put: element.
		index <- index + 1 ].
	^ newArray
!
METHOD Collection
from: argLow to: argHigh | ret idx size base low high |
	low <- argLow max: 1.
	high <- argHigh min: self size.
	size <- (high - low) + 1.
	(size < 1) ifTrue: [ ^ Array new: 0 ].
	ret <- Array new: size.
	base <- idx <- 1.
	self do: [:elem|
		((idx >= low) and: [idx <= high]) ifTrue: [
			ret at: base put: elem.
			base <- base + 1.
			(base > size) ifTrue: [ ^ ret ]
		].
		idx <- idx + 1.
	].
	^ ret
!
METHOD Collection
from: argLow
	^ self from: argLow to: (self size)
!
METHOD Collection
asString	| newString index |
	newString <- String new: self size.
	index <- 1.
	self do: [ :element | newString at: index put: element.
		index <- index + 1 ].
	^ newString
!
METHOD Collection
asList
	^ List new addAll: self
!
METHOD Collection
collect: transformBlock	| newList |
	newList <- List new.
	self do: [:element | newList addLast: (transformBlock value: element)].
	^ newList
!
METHOD Collection
select: testBlock	| newList |
	newList <- List new.
	self do: [:x | (testBlock value: x) ifTrue: [newList addLast: x]].
	^ newList
!
METHOD Collection
< aCollection
	self do: [ :element | (aCollection includes: element)
		ifFalse: [ ^ false ] ].
	^ true
!
METHOD Collection
= aCollection
	^ self < aCollection and: [ aCollection < self ]
!
METHOD Collection
reject: testBlock
		" select the things that do not match predicate "
	^ self select: [:x | (testBlock value: x) not ]
!
METHOD Collection
printString | count res |
	res <- super printString.
	(self respondsTo: #do:) ifFalse: [ ^ res ].
	count <- 0.
	res <- res + ' ('.
	self basicDo: [:elem| 
		(count = 0) ifFalse: [ res <- res + ' ' ].
		res <- res + elem printString.
		count <- count + 1.
		(count >= 20) ifTrue: [ ^ res + ' ...)' ]
	].
	^ res + ')'
!
METHOD Collection
occurencesOf: obj | count |
	count <- 0.
	self do: [:o| (o = obj) ifTrue: [ count <- count + 1]].
	^ count
!
METHOD Collection
anyOne
	self do: [:it| ^ it].
	self emptyCollection
!
METHOD Collection
emptyCollection
	self error: (self class printString + ' is empty')
!
METHOD Collection
do: aBlock
	self subclassResponsibility
!
COMMENT ---------- Array ------------
METHOD MetaArray
new
	^ self new: 0
!
METHOD MetaArray
new: sz
	<7 self sz>
!
METHOD MetaArray
with: elemA | ret |
	ret <- self new: 1.
	ret at: 1 put: elemA.
	^ ret
!
METHOD MetaArray
with: elemA with: elemB | ret |
	ret <- self new: 2.
	ret at: 1 put: elemA.
	ret at: 2 put: elemB.
	^ ret
!
METHOD MetaArray
with: elemA with: elemB with: elemC | ret |
	ret <- self new: 3.
	ret at: 1 put: elemA.
	ret at: 2 put: elemB.
	ret at: 3 put: elemC.
	^ ret
!
METHOD Array
badIndex
	self error: 'array indexing error'
!
METHOD Array
at: index
	<24 self index>
	(self includesKey: index) ifFalse: [ self badIndex ].
	self primitiveFailed
!
METHOD Array
at: index ifAbsent: exceptionBlock
	<24 self index>
	exceptionBlock value
!
METHOD Array
includes: aValue
	self do: [ :element | element = aValue ifTrue: [ ^ true ]].
	^ false
!
METHOD Array
indexOf: aValue
	1 to: self size do: [:idx|
		((self at: idx) == aValue) ifTrue: [ ^ idx ]
	].
	^ nil
!
METHOD Array
indexOfVal: aValue
	1 to: self size do: [:idx|
		((self at: idx) = aValue) ifTrue: [ ^ idx ]
	].
	^ nil
!
METHOD Array
copy
	^ self asArray
!
METHOD Array
with: newItem	| newArray size |
	size <- self size.
	newArray <- self class new: size + 1.
	newArray replaceFrom: 1 to: size with: self.
	newArray at: size + 1 put: newItem
	^ newArray
!
METHOD Array
+ aValue	| size1 size2 newValue |
	" catenate two strings together "
	size1 <- self size.
	size2 <- aValue size.
	newValue <- self class new: (size1 + size2).
	newValue replaceFrom: 1 to: size1 with: self.
	newValue replaceFrom: size1+1 to: size1+size2 with: aValue.
	^ newValue
!
METHOD Array
size
	" compute number of elements "
	<4 self>
!
METHOD Array
at: index put: value
	<5 value self index>
	(self includesKey: index) ifFalse: [ self badIndex ].
	self primitiveFailed
!
METHOD Array
do: aBlock
	1 to: self size do: [:i | aBlock value: (self at: i)]
!
METHOD Array
< arg		| selfsize argsize |
	selfsize <- self size. argsize <- arg size.
	1 to: (selfsize min: argsize)
		do: [:i | (self at: i) ~= (arg at: i)
			ifTrue: [ ^ (self at: i) < (arg at: i) ]].
	^ selfsize < argsize
!
METHOD Array
= anArray
	self size = anArray size ifFalse: [ ^ false ].
	1 to: self size do:
		[:i | (self at: i) = (anArray at: i)
			ifFalse: [ ^ false ]].
	^ true
!
METHOD Array
includesKey: index
	^ index between: 1 and: self size
!
METHOD Array
insert: value at: position | newArray newSize |
	newSize <- self size + 1.
	newArray <- self class new: newSize.
	newArray replaceFrom: 1 to: position-1 with: self.
	newArray at: position put: value.
	newArray replaceFrom: position+1 to: newSize with:
		self startingAt: position.
	^ newArray
!
METHOD Array
removeIndex: position  | newArray newSize |
	newSize <- self size - 1.
	newArray <- self class new: newSize.
	newArray replaceFrom: 1 to: position-1 with: self.
	newArray replaceFrom: position to: newSize with: self
		startingAt: position+1.
	^ newArray
!
METHOD Array
first
	^self at: 1
!
METHOD Array
from: low to: high | start stop size obj |
	start <- low max: 0.
	stop <- high min: self size.
	size <- (stop + 1 - start) max: 0.
	obj <- (self species) new: size.
	1 to: size do: [ :i |
		obj at: i put: (self at: start).
		start <- start + 1 ].
	^ obj
!
METHOD Array
hash | sz |
	sz <- self size.
	(sz < 2) ifTrue: [
		(sz = 1) ifTrue: [ ^ (self at: 1) hash + sz ].
		^ 0
	].
	^ (self at: 1) hash + (self at: sz) hash
!
METHOD Array
elementsExchangeIdentityWith: otherArray
	<35 self otherArray>.
	self primitiveFailed
!
METHOD Array
replaceFrom: start to: stop with: replacement
	^ self replaceFrom: start to: stop with: replacement startingAt: 1
!
METHOD Array
replaceFrom: start to: stop with: replacement startingAt: repStart | base |
	<38 start stop replacement repStart self>.
	base <- repStart-1.
	0 to: (stop - start) do: [:idx|
		self at: (idx + start) put:
			(replacement at: (idx + repStart))
	]
!
COMMENT ---------- OrderedArray ------------
METHOD OrderedArray
add: value
	^ self insert: value at: (self location: value)
!
METHOD OrderedArray
includes: value | position |
	position <- self location: value.
	^ (position <= self size) and: [ value = (self at: position)]
!
METHOD OrderedArray
location: value | low high mid |
	low <- 1.
	high <- self size + 1.
	[ low < high ] whileTrue:
		[ mid <- (low + high) quo: 2.
		(self at: mid) < value
			ifTrue: [ low <- mid + 1 ]
			ifFalse: [ high <- mid ] ].
	^ low
!
COMMENT ---------- ByteArrays ------------
METHOD MetaByteArray
new: size
	<20 self size>
!
METHOD ByteArray
basicAt: index
	<21 self index>
	^nil
!
METHOD ByteArray
at: index
	<21 self index>
	(self includesKey: index) ifFalse: [ self badIndex ].
	self primitiveFailed
!
METHOD ByteArray
at: index ifAbsent: exceptionBlock
	<21 self index>
	exceptionBlock value
!
METHOD ByteArray
at: index put: aValue
	<22 aValue self index>
	(self includesKey: index) ifFalse: [ self badIndex ].
	self primitiveFailed
!
METHOD ByteArray
asString | str sz |
	sz <- self size.
	str <- String new: sz.
	1 to: sz do: [:i| str at: i put: ((self at: i) asChar)].
	^ str
!
COMMENT ---------- Strings ------------
METHOD MetaString
new: size
	<20 self size>
!
METHOD MetaString
input	| value c nl |
	" read a line from input "
	value <- ''. nl <- Char newline.
	[ c <- Char input.
	  c isNil ifTrue: [ ^ nil ]. c ~= nl ] whileTrue:
		[ value <- value + c asString ].
	^ value
!
METHOD MetaString
newline
	^ Char newline asString
!
METHOD MetaString
tab
	^ Char tab asString
!
METHOD String
edit
	<105 self>
!
METHOD String
break: separators  | words word |
	" break string into words, using separators "
	word <- ''.
	words <- List new.
	self do: [:c |
		(separators includes: c)
			ifTrue: [
				(word size > 0) " found a word "
					ifTrue: [ words addLast: word.
							word <- '' ] ]
			ifFalse: [ word <- word + c asString ] ].
		" maybe a last word "
	(word size > 0) ifTrue: [ words addLast: word ].
	^ words
!
METHOD String
collect: transformationBlock
	^ (super collect: transformationBlock) asString
!
METHOD String
reverse
	^ self asList reverse asString
!
METHOD String
select: testBlock
	^ (super select: testBlock) asString
!
METHOD String
printString
	^ self
!
METHOD String
printWidth: width | ret |
	(self size >= width absolute) ifTrue: [ ^ self ].
	ret <- self.
	(width negative) ifTrue: [
			(self size + 1) to: (width negated) do:
				[:ignore| ret <- ' ' + ret].
		]
		ifFalse: [
			(self size + 1) to: width do:
				[:ignore| ret <- ret + ' ' ].
		].
	^ret
!
METHOD String
asSymbol
	^ Symbol new: self
!
METHOD String
doIt	| meth |
	meth <- Undefined parseMethod: 'doItCommand ^' + self.
	^ meth notNil
		ifTrue: [ ^ Context new
			  perform: meth withArguments: (Array new: 1) ]
!
METHOD String
basicAt: index
	<21 self index>
	^nil
!
METHOD String
at: index
	^self at: index ifAbsent: [ self badIndex ]
!
METHOD String
at: index ifAbsent: exceptionBlock | c |
	c <- self basicAt: index.
	(c isNil)
	     ifTrue: [ ^ exceptionBlock value ]
	     ifFalse: [ ^ Char new: c ]
!
METHOD String
basicAt: index put: value
	<22 value self index>
	^nil
!
METHOD String
at: index put: aValue
	(self basicAt: index put: aValue value) isNil ifTrue: [
		self badIndex
	]
!
METHOD String
copy
	" make a clone of ourself "
	<23 self String>
!
METHOD String
asNumber | val |
	" parse a base-10 ASCII number, return nil on failure "
	val <- 0.
	self do: [:c|
		c isDigit ifFalse: [^nil].
		val <- (val * 10) + (c value - 48)
	].
	^val
!
METHOD String
hash | sz |
	sz <- self size.
	(sz < 2) ifTrue: [
		(sz = 1) ifTrue: [ ^ (self at: 1) value ].
		^ 0
	].
	^ (self at: 1) value + (self at: sz) value
!
METHOD String
lowerCase | ret c |
	ret <- String new: (self size).
	1 to: self size do: [:x|
		c <- self at: x.
		c isUpperCase ifTrue: [c <- c lowerCase].
		ret at: x put: c
	].
	^ ret
!
METHOD String
indexOf: val | c s |
	" For non-strings, search scalar Array elements "
	(val isKindOf: String) ifFalse: [ ^ super indexOf: val ].

	" Otherwise look for substring match "
	c <- val at: 1.
	s <- val size.
	1 to: (self size - s + 1) do: [:idx|
		" Cheap check for initial char equality "
		((self at: idx) = c) ifTrue: [
			((self from: idx to: (idx + s - 1)) = val)
				ifTrue: [ ^ idx ]
		]
	].
	^ nil
!
COMMENT --------- List -----------
METHOD MetaList
with: elemA	| newList |
	newList <- self new.
	newList add: elemA.
	^ newList
!
METHOD MetaList
with: elemA with: elemB	| newList |
	newList <- self new.
	newList add: elemB.  newList add: elemA.
	^ newList
!
METHOD MetaList
with: elemA with: elemB with: elemC	| newList |
	newList <- self new.
	newList add: elemC.  newList add: elemB.  newList add: elemA.
	^ newList
!
METHOD List
add: anElement
	elements <- Link value: anElement next: elements.
	^ anElement
!
METHOD List
addLast: anElement
	elements isNil
		ifTrue: [ self add: anElement]
		ifFalse: [ elements addLast: anElement ].
	^ anElement
!
METHOD List
addAll: aCollection
	aCollection do: [ :element | self addLast: element ]
!
METHOD List
+ aCollection
	^ self addAll: aCollection
!
METHOD List
copy
	^ self asList
!
METHOD List
badIndex
	self error: 'Invalid List index'
!
METHOD List
findLink: index ifAbsent: aBlock | idx link |
	link <- elements.
	idx <- index.
	link isNil ifTrue: [ ^ aBlock value ].
	[ link notNil ] whileTrue: [
		idx <- idx-1.
		(idx = 0) ifTrue: [ ^ link ].
		link <- link next
	].
	^ aBlock value
!
METHOD List
at: index | link |
	link <- self findLink: index ifAbsent: [ self badIndex ].
	^ link value
!
METHOD List
at: index ifAbsent: aBlock | link |
	link <- self findLink: index ifAbsent: [nil].
	link isNil ifTrue: [ ^ aBlock value ].
	^ link value
!
METHOD List
at: index put: value | link |
	link <- self findLink: index.
	link value: value
!
METHOD List
first
	^ self at: 1
!
METHOD List
isEmpty
	^ elements isNil
!
METHOD List
removeFirst
	elements isNil
		ifTrue: [ self emptyCollection ]
		ifFalse: [ elements <- elements next ]
!
METHOD List
remove: anElement
	self remove: anElement
		ifAbsent: [ self emptyCollection ]
!
METHOD List
reverse | newList |
	newList <- List new.
	self do: [ :element | newList add: element ].
	^ newList
!
METHOD List
remove: anElement ifAbsent: exceptionBlock
	elements isNil
		ifTrue: [ exceptionBlock value ]
		ifFalse: [ elements remove: anElement ifAbsent: exceptionBlock ]
!
METHOD List
do: aBlock
	^ elements notNil ifTrue: [ elements do: aBlock ]
!
METHOD List
reverseDo: aBlock
	^ elements notNil ifTrue: [ elements reverseDo: aBlock ]
!
METHOD List
select: testBlock | newList |
	newList <- List new.
	self reverseDo: [:element | (testBlock value: element)
		ifTrue: [ newList add: element ] ].
	^ newList
!
COMMENT ---------- Dictionary ------------
METHOD MetaDictionary
new | newDict |
	newDict <- super new.
	self in: newDict at: 1 put: (OrderedArray new: 0).
	self in: newDict at: 2 put: (Array new: 0).
	^ newDict
!
METHOD Dictionary
noKey
	self error: 'key not found in dictionary lookup'
!
METHOD Dictionary
at: key
	^ self at: key ifAbsent: [ self noKey ]
!
METHOD Dictionary
at: key put: value | position |
	position <- keys location: key.
	(position <= keys size and: [ key = (keys at: position)])
		ifTrue: [ values at: position put: value ]
		ifFalse: [ keys <- keys insert: key at: position.
			values <- values insert: value at: position ].
	^ value
!
METHOD Dictionary
at: key ifAbsent: exceptionBlock | position |
	position <- keys location: key.
	((position <= keys size) and: [ key = (keys at: position)])
		ifTrue: [ ^ values at: position ]
		ifFalse: [ ^ exceptionBlock value ]
!
METHOD Dictionary
binaryDo: aBlock
	1 to: keys size do:
		[:i | aBlock value: (keys at: i) value: (values at: i) ]
!
METHOD Dictionary
keysDo: aBlock
	1 to: keys size do: [:i| aBlock value: (keys at: i)]
!
METHOD Dictionary
keysAsArray | i ret |
	ret <- Array new: keys size.
	1 to: keys size do: [:i| ret at: i put: (keys at: i)].
	^ ret
!
METHOD Dictionary
isEmpty
	^ keys isEmpty
!
METHOD Dictionary
do: aBlock
	values do: aBlock
!
METHOD Dictionary
removeKey: key ifAbsent: exceptionBlock | position |
	position <- keys location: key.
	(position <= keys size and: [ key = (keys at: position) ])
		ifTrue: [ keys <- keys removeIndex: position.
			values <- values removeIndex: position]
		ifFalse: [ ^ exceptionBlock value ]
!
METHOD Dictionary
removeKey: key
	^ self removeKey: key ifAbsent: [ self noKey ]
!
METHOD Dictionary
printString | count res |
	res <- self class printString + ' ('.
	count <- 0.
	self binaryDo: [:k :elem| 
		(count = 0) ifFalse: [ res <- res + ', ' ].
		res <- res + (k printString + ' -> ' + elem printString).
		count <- count + 1.
		(count >= 20) ifTrue: [ ^ res + ', ...)' ]
	].
	^ res + ')'
!
METHOD Dictionary
add: anAssoc
	self at: anAssoc key put: anAssoc value
!
COMMENT ---------- Set ------------
METHOD MetaSet
new: size | ret |
	ret <- super new.
	self in: ret at: 1 put: (Array new: size).
	self in: ret at: 2 put: size.
	^ ret
!
METHOD MetaSet
with: objA | ret |
	ret <- self new.
	ret add: objA.
	^ ret
!
METHOD MetaSet
with: objA with: objB | ret |
	ret <- self new.
	ret add: objA. ret add: objB.
	^ ret
!
METHOD MetaSet
with: objA with: objB with: objC | ret |
	ret <- self new.
	ret add: objA. ret add: objB. ret add: objC.
	^ ret
!
METHOD MetaSet
new
	^ self new: 10
!
METHOD Set
size | tally |
	tally <- 0.
	members do: [:elem| elem notNil ifTrue: [ tally <- tally + 1 ] ].
	^ tally
!
METHOD Set
grow | bigger old oldsize |
	" Re-create ourselves in place with a new, bigger storage "
	old <- members.
	members <- Array new: (old size + growth).

	" Re-insert each existing Set member "
	old do: [:elem| self add: elem]
!
METHOD Set
compare: t and: e
	^ t = e
!
METHOD IdentitySet
compare: t and: e
	^ t == e
!
METHOD Set
location: elem | pos start t |
	start <- pos <- (elem hash rem: members size) + 1.
	[ true ] whileTrue: [
		" Return this position if we match, or have reached
		  a nil slot. "
		t <- members at: pos.
		((t isNil) or: [self compare: t and: elem]) ifTrue: [
			^ pos
		].

		" Advance to next slot, circularly "
		pos <- pos + 1.
		(pos > members size) ifTrue: [
			pos <- 1
		].

		" Return nil if we have scanned the whole Set "
		(pos = start) ifTrue: [ ^ nil ]
	]
!
METHOD Set
add: elem | pos |
	" Find the appropriate slot... if none, need to grow the Set "
	pos <- self location: elem.
	pos isNil ifTrue: [
		self grow.
		^ self add: elem
	].

	" If the slot is nil, this is a new entry which we put in place now.
	  If it wasn't nil, we still re-store it so that if it's an
	  Association, the value portion will be updated. "
	members at: pos put: elem.
	^ elem
!
METHOD Set
rehash: start | pos elem |
	pos <- start.
	[ true ] whileTrue: [
		" Advance to next slot, ceasing when we reach our start "
		pos <- pos + 1.
		(pos > members size) ifTrue: [ pos <- 1 ].
		(pos = start) ifTrue: [ ^ self ]

		" If we reach a nil slot, there are no further rehash
		  worries. "
		elem <- members at: pos.
		elem isNil ifTrue: [ ^ self ].

		" Nil out the slot, and then re-insert the element "
		members at: pos put: nil.
		self add: elem
	]
!
METHOD Set
remove: elem ifAbsent: aBlock | pos |
	" If not found, return error "
	pos <- self location: elem.
	((pos isNil) or: [(members at: pos) isNil]) ifTrue: [
		aBlock value
	].

	" Remove our element from the Set "
	members at: pos put: nil.

	" Re-hash all that follow "
	self rehash: pos.

	^ elem
!
METHOD Set
remove: elem
	^ self remove: elem ifAbsent: [self noElement ]
!
METHOD Set
do: aBlock
	members do: [:elem| elem notNil ifTrue: [ aBlock value: elem ]]
!
METHOD Set
at: value ifAbsent: aBlock | pos |
	pos <- self location: value.
	((pos isNil) or: [ (members at: pos) isNil ]) ifTrue: [
		^ aBlock value
	].
	^ value
!
METHOD Set
indexOf: value
	^ self at: value ifAbsent: [ nil ]
!
COMMENT ---------- Number ------------
METHOD MetaNumber
new
	" can't create this way, return zero "
	^ 0
!
METHOD Number
negative
	^self < 0
!
METHOD Number
absolute
	(self negative) ifTrue: [ ^ self negated ]
!
METHOD Number
negated
	^0-self
!
METHOD Number
factorial
	self <= 1 ifTrue: [ ^ 1 ]
	ifFalse: [ ^ (self - 1) factorial * self ]
!
METHOD Number
asDigit
	(self < 10) ifTrue:
		[ ^(Char new: (self asSmallInt + 48)) asString ].
	^(Char new: (self asSmallInt + 55)) asString
!
METHOD Number
printWidth: width base: base | res n dig wasNeg wide |
	res <- ''.
	(self negative) ifTrue: [
		wasNeg <- true.
		wide <- width-1.
		n <- self negated
	] ifFalse: [
		wasNeg <- false.
		wide <- width.
		n <- self
	].
	[true] whileTrue: [
		res <- ((n rem: base) asDigit) + res.
		n <- n quo: base.
		(n = 0) ifTrue: [
			((res size)+1) to: wide do: [:ignore|
				res <- '0' + res
			].
			wasNeg ifTrue: [ res <- '-' + res ].
			^res
		]
	]
!
METHOD Number
printWidth: width
	^self printWidth: width base: 10
!
METHOD Number
printString
	^self printWidth: 1 base: 10
!
METHOD Number
to: limit
	^ Interval from: self to: limit step: 1
!
METHOD Number
to: limit by: step
	^ Interval from: self to: limit step: step
!
METHOD Number
to: limit do: aBlock  | i |
		" optimize arithmetic loops "
	i <- self.
	[ i <= limit ] whileTrue: [ aBlock value: i. i <- i + 1 ]
!
METHOD Number
to: limit by: step do: aBlock  | i |
	i <- self.
	[ i <= limit ] whileTrue: [ aBlock value: i. i <- i + step ]
!
METHOD Number
overflow
	self error: 'Numeric overflow'
!
METHOD Number
bitAnd: arg
	^ (self asSmallInt bitAnd: arg)
!
METHOD Number
bitOr: arg
	^ (self asSmallInt bitOr: arg)
!
METHOD Number
bitShift: arg
	^ (self asSmallInt bitShift: arg)
!
METHOD Number
atRandom
	" Return random number from 1 to self "
	(self < 2) ifTrue: [ ^ self ].
	^ ((1 to: self) atRandom)
!
METHOD Number
asChar
	^ Char new: (self asSmallInt)
!
COMMENT ---------- SmallInt ------------
METHOD MetaSmallInt
atRandom
	" Set up seed one time.  TBD: init from something external;
	  getpid() or time() "
	seed isNil ifTrue: [ seed <- 17 ].

	" Rotate the random number generator. "
	seed <- ((seed * 1103515245 + 12345) truncSmallInt)
		bitAnd: 268435455.
	^ seed
!
METHOD SmallInt
asSmallInt
	^self
!
METHOD SmallInt
truncSmallInt
	^self
!
METHOD SmallInt
asInteger
	^Integer new: self
!
METHOD SmallInt
quo: arg
	<11 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self quo: arg asSmallInt].
	(0 = arg) ifTrue: [^ self error: 'division by zero'].
	self primitiveFailed
!
METHOD SmallInt
rem: arg
	<12 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self rem: arg asSmallInt].
	(0 = arg) ifTrue: [^ self error: 'division by zero'].
	self primitiveFailed
!
METHOD SmallInt
+ arg
	<10 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self + arg asSmallInt].
	self primitiveFailed
!
METHOD SmallInt
* arg
	<15 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self * arg asSmallInt].
	self primitiveFailed
!
METHOD SmallInt
- arg
	<16 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self - arg asSmallInt].
	self primitiveFailed
!
METHOD SmallInt
< arg
	<13 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self < arg asSmallInt].
	self primitiveFailed
!
METHOD SmallInt
= arg
	<14 self arg>
	(arg isMemberOf: SmallInt) ifFalse: [^self = arg asSmallInt].
	self primitiveFailed
!
METHOD SmallInt
hash
	^ self
!
METHOD SmallInt
bitOr: arg
	<36 self arg>.
	^ (self bitOr: arg asSmallInt)
!
METHOD SmallInt
bitAnd: arg
	<37 self arg>.
	^ (self bitAnd: arg asSmallInt)
!
METHOD SmallInt
bitShift: arg
	<39 self arg>.
	(arg isKindOf: SmallInt) ifTrue: [ self overflow ].
	^ (self bitShift: arg asSmallInt)
!
COMMENT ---------- Integer ------------
METHOD MetaInteger
new: low
	<32 low>
	low <- low asSmallInt.
	<32 low>
	self primitiveFailed
!
METHOD Integer
asInteger
	^self
!
METHOD Integer
asSmallInt
	<33 self>.
	self overflow
!
METHOD Integer
truncSmallInt
	<40 self>.
	self primitiveFailed
!
METHOD Integer
hash
	<33 self>.
	^ (self rem: 65536) asSmallInt
!
METHOD Integer
quo: arg
	<25 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self quo: arg asInteger].
	(0 = arg) ifTrue: [^ self error: 'division by zero'].
	self primitiveFailed
!
METHOD Integer
rem: arg
	<26 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self rem: arg asInteger].
	(0 = arg) ifTrue: [^ self error: 'division by zero'].
	self primitiveFailed
!
METHOD Integer
+ arg
	<27 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self + arg asInteger].
	self primitiveFailed
!
METHOD Integer
* arg
	<28 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self * arg asInteger].
	self primitiveFailed
!
METHOD Integer
- arg
	<29 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self - arg asInteger].
	self primitiveFailed
!
METHOD Integer
< arg
	<30 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self < arg asInteger].
	self primitiveFailed
!
METHOD Integer
= arg
	<31 self arg>
	(arg isMemberOf: Integer) ifFalse: [^self = arg asInteger].
	self primitiveFailed
!
COMMENT ---------- Nodes ------------
METHOD MetaNode
new: value
	" creation, left left and right empty "
	^ self in: self new at: 1 put: value
!
METHOD Node
do: aBlock
	left notNil ifTrue: [ left do: aBlock ].
	aBlock value: value.
	^ right notNil ifTrue: [ right do: aBlock ]
!
METHOD Node
reverseDo: aBlock
	right notNil ifTrue: [ right do: aBlock ].
	aBlock value: value.
	left notNil ifTrue: [ left do: aBlock ]
!
METHOD Node
first
	left notNil
		ifTrue: [ ^ left first ]
		ifFalse: [ ^ value ]
!
METHOD Node
removeFirst
	left notNil
		ifTrue: [ left <- left removeFirst. ^ self ]
		ifFalse: [ ^ right ]
!
METHOD Node
add: anElement
	value < anElement
		ifTrue: [ right notNil
			ifTrue: [ right add: anElement ]
			ifFalse: [ right <- Node new: anElement ] ]
		ifFalse: [ left notNil
			ifTrue: [ left add: anElement ]
			ifFalse: [ left <- Node new: anElement ] ]
!
METHOD Node
remove: key ifAbsent: exceptionBlock
	value = key
		ifTrue: [ right notNil
			ifTrue: [ value <- right first.
			right <- right removeFirst.
			^ self ]
			ifFalse: [ ^ left ] ].
	value < key
		ifTrue: [ right notNil
			ifTrue: [ right <- right remove: key ifAbsent: exceptionBlock ]
			ifFalse: [ ^ exceptionBlock value ] ]
		ifFalse: [ left notNil
			ifTrue: [ left <- left removeL key ifAbsent: exceptionBlock ]
			ifFalse: [ ^ exceptionBlock value ] ]
!
METHOD Node
value
	^ value
!
METHOD Node
at: key ifAbsent: exceptionBlock
	value = key ifTrue: [ ^ value ].
	value < key
		ifTrue: [ right notNil
			ifTrue: [ ^ right at: key ifAbsent: exceptionBlock ]
			ifFalse: [ ^ exceptionBlock value ] ]
		ifFalse: [ left notNil
			ifTrue: [ ^ left at: key ifAbsent: exceptionBlock ]
			ifFalse: [ ^ exceptionBlock value ] ]
!
COMMENT ---------- Intervals ------------
METHOD MetaInterval
from: l to: h step: s | newInterval |
	newInterval <- self in: self new at: 1 put: l.
	self in: newInterval at: 2 put: h.
	self in: newInterval at: 3 put: s.
	^ newInterval
!
METHOD Interval
do: aBlock	| current |
	current <- low.
	(step < 0)
		ifTrue: [
			[ current >= high ] whileTrue:
				[ aBlock value: current.
				current <- current + step ] ]
		ifFalse: [
			[ current <= high ] whileTrue:
				[ aBlock value: current.
				current <- current + step ] ]
!
METHOD Interval
low: l
	low <- l
!
METHOD Interval
high: h
	high <- h
!
METHOD Interval
low
	^ low
!
METHOD Interval
high
	^ high
!
METHOD Interval
includes: val
	" Check within range first "
	((val < low) or: [val > high]) ifTrue: [ ^ false ].
	" Then check if in equivalence class of interval "
	^ ((val - low) rem: step) = 0
!
METHOD Interval
printString | s |
	s <- (self class printString) + ' <' +
		low printString + '..' + high printString.
	(step ~= 1) ifTrue: [ s <- s + ' by ' + step printString ].
	^ s + '>'
!
METHOD Interval
atRandom | ret |
	" Return a random element from our sequence "
	ret <- (SmallInt atRandom) rem: ((high - low + 1) quo: step).
	^ low + (ret * step)
!
COMMENT ---------- Links ------------
METHOD MetaLink
value: v
		" return a new link with given value field "
		" and empty link field "
	^ self in: self new at: 1 put: v
!
METHOD MetaLink
value: v next: n	| new |
		" return a new link with the given fields "
	new <- self new.
	self in: new at: 1 put: v.
	self in: new at: 2 put: n.
	^ new
!
METHOD Link
value
	^ value
!
METHOD Link
value: val
	value <- val
!
METHOD Link
next
	^ next
!
METHOD Link
do: aBlock
	aBlock value: value.
	next notNil ifTrue: [ ^ next do: aBlock ]
!
METHOD Link
remove: anElement ifAbsent: exceptionBlock
	value = anElement
		ifTrue: [ ^ next ]
		ifFalse: [ next notNil
			ifTrue: [ next <- next remove: anElement
				ifAbsent: exceptionBlock. ^ self ]
			ifFalse: [ ^ exceptionBlock value ] ]
!
METHOD Link
reverseDo: aBlock
	next notNil ifTrue: [ next reverseDo: aBlock ].
	aBlock value: value
!
METHOD Link
addLast: anElement
	next notNil
		ifTrue: [ ^ next addLast: anElement ]
		ifFalse: [ next <- Link value: anElement ]
!
COMMENT ---------- Association ------------
METHOD MetaAssociation
key: k
		"key is set once, value is resettable"
	^ self in: self new at: 1 put: k
!
METHOD MetaAssociation
key: k value: v | ret |
		"key is set once, value is resettable"
	ret <- self new.
	self in: ret at: 1 put: k.
	self in: ret at: 2 put: v.
	^ ret
!
METHOD Association
= k
		"compare both with keys and associations"
	(k class == Association)
		ifTrue: [ ^ key = k key ]
		ifFalse: [ ^ key = k ]
!
METHOD Association
< k
		"compare both with keys and associations"
	(k class == Association)
		ifTrue: [ ^ key < k key ]
		ifFalse: [ ^ key < k ]
!
METHOD Association
value: v
	value <- v
!
METHOD Association
value
	^ value
!
METHOD Association
key
	^ key
!
METHOD Association
hash
	^ key hash
!
METHOD Association
printString
	^ '(' + key printString + ' -> ' + value printString + ')'
!
COMMENT ---------- Tree ------------
METHOD Tree
add: anElement
	root isNil
		ifTrue: [ root <- Node new: anElement ]
		ifFalse: [ root add: anElement ].
	^anElement
!
METHOD Tree
addAll: aCollection
	aCollection do: [:element| self add: element ]
!
METHOD Tree
at: key ifAbsent: exceptionBlock
	root isNil
		ifTrue: [ ^ exceptionBlock value ]
		ifFalse: [ ^ root at: key ifAbsent: exceptionBlock ]
!
METHOD Tree
copy
	^Tree new addAll: self
!
METHOD Tree
collect: transformBlock | newTree |
	newTree <- Tree new.
	self do: [:element| newTree add: (transformBlock value: element)]
	^newTree
!
METHOD Tree
do: aBlock
	root notNil ifTrue: [ root do: aBlock ]
!
METHOD Tree
select: testBlock | newTree |
	newTree <- Tree new.
	self do: [:element|
		(testBlock value: element)
			ifTrue: [newTree add: element]
	].
	^newTree
!
METHOD Tree
reverseDo: aBlock
	root notNil ifTrue: [ root reverseDo: aBlock ]
!
METHOD Tree
removeFirst
	root isNIl ifTrue: [ self emptyCollection ].
	root <- root removeFirst
!
METHOD Tree
remove: key ifAbsent: exceptionBlock
	root isNil
		ifTrue: [ exceptionBlock value ]
		ifFalse: [ root <- root remove: key ifAbsent: exceptionBlock ]
!
METHOD Tree
first
	root notNil
		ifTrue: [ ^root first ]
		ifFalse: [ self emptyCollection ]
!
METHOD Tree
isEmpty
	^ root isNil
!
COMMENT --------------file methods-----------------
METHOD MetaFile
doOpen: nm mode: mode
	<100 nm mode>
!
METHOD MetaFile
openRead: nm
		" open new file for reading "
	^ self in: (self new) at: 1 put: (self doOpen: nm mode: 'r')
!
METHOD MetaFile
openWrite: nm
		" open new file for writing "
	^ self in: (self new) at: 1 put: (self doOpen: nm mode: 'w')
!
METHOD MetaFile
openUpdate: nm
		" open new file for reading and writing "
	^ self in: (self new) at: 1 put: (self doOpen: nm mode: 'r+')
!
METHOD MetaFile
fileIn: nm | file |
	file <- self openRead: nm.
	file opened ifFalse: [ ^ self error: 'cannot open file ' + nm ].
	file fileIn.
	file close.
	^ 'file in completed'
!
METHOD MetaFile
image: nm | file |
		" open a file, write the image, then close "
	file <- self openWrite: nm.
	file opened ifFalse: [ ^ self error: 'cannot open file ' + nm ].
	file writeImage.
	file close
!
METHOD File
opened
	^ fileID notNil
!
METHOD File
close: id
	<103 id>
!
METHOD File
close
		" close file, return file descriptor "
	fileID notNil ifTrue: [
		self close: fileID.
		fileID <- nil
	]
!
METHOD File
writeImage
		" save the current image in a file "
	fileID notNil
		ifTrue: [ <104 fileID> ]
!
METHOD File
notOpened
	self error: 'file is not open'
!
METHOD File
writeCharValue: n
	<102 fileID n>.
	fileID isNil ifTrue: [ self notOpened ].
	self primitiveFailed
!
METHOD File
doRead
	<101 fileID>.
	fileID isNil ifTrue: [ self notOpened ].
	self primitiveFailed
!
METHOD File
readChar	| c |
		" read a single character from a file "
	c <- self doRead.
	c notNil ifTrue: [ ^ Char new: c ].
	^ c
!
METHOD File
readLine	| value  c nl |
	" read a line from input "
	fileID isNil ifTrue: [ self error: 'cannot read from unopened file' ].
	value <- ''.
	nl <- Char newline.
	[ c <- self doRead.
	  c isNil ifTrue: [ ^ nil ].
	  c <- Char new: c.
	  c ~= nl ] whileTrue:
		[ value <- value + c asString ].
	^ value
!
METHOD File
fileInDispatch: cmd | c |
	" Immediate execte "
	cmd = $+ ifTrue: [
		self readLine doIt printNl.
		^ self
	].

	" Method definition "
	(cmd = $! or: [ cmd = $=]) ifTrue: [
		self methodCommand: cmd = $!.
		^ self
	].

	" Comment enclosed in quotes... find matching quote "
	(cmd = $") ifTrue: [
		[ c <- self readChar. c ~= $" ] whileTrue: [
			" Consume chars until closing quote "
			nil
		].
		^ self
	].

	" Blank line, just return to process next line "
	(cmd = Char newline) ifTrue: [
		^ self
	].

	" It is random chars (treat as comment--discard) "
	self readLine
!
METHOD File
fileIn		| cmd |
	[ cmd <- self readChar. cmd notNil ] whileTrue: [
		self fileInDispatch: cmd
	]
!
METHOD File
methodCommand: classCmd | name aClass text line |
	name <- self readLine asSymbol.
	aClass <- globals at: name ifAbsent: [ ^ self error:
		'unknown class name in file-in: ' + name printString ].
	text <- ''.
	[ line <- self readLine.
	  line isNil ifTrue: [ ^ self error:
		'unexpected end of input during fileIn' ].
	  line ~= '!'] whileTrue: [ text <- text + line + String newline ].
	classCmd
		ifTrue: [ aClass addMethod: text ]
		ifFalse: [ aClass class addMethod: text ]
!
METHOD File
at: idx
	<108 fileID idx>.
	self primitiveFailed
!
METHOD File
write: buf size: count
	<107 fileID buf count>.
	self primitiveFailed
!
METHOD File
write: buf
	^ self write: buf size: (buf size)
!
METHOD File
at: idx put: buf
	self at: idx.
	self write: buf size: buf size
!
METHOD File
at: idx get: buf | size |
	self at: idx.
	size <- buf size.
	<106 fileID buf size>
!
METHOD File
at: idx size: count | buf res |
	buf <- ByteArray new: count.
	res <- self at: idx get: buf.
	(res < count) ifTrue: [ buf <- buf from: 1 to: res ].
	^ buf
!
METHOD File
newline
	^ self write: (String newline)
!
COMMENT --------------parser methods-----------------
METHOD Parser
text: aString instanceVars: anArray
	text <- aString.
	index <- 1.
	lineNum <- 1.
	argNames <- Array new: 1.
	argNames at: 1 put: #self.
	instNames <- anArray.
	tempNames <- Array new: 0.
	maxTemps <- 0
!
METHOD Parser
parse: c with: encoderClass	| encoder meth name |
	" note -- must call text:instanceVars: first "
	errBlock <- [ ^ nil ].
	self nextLex.
	encoder <- encoderClass new.
	name <- self readMethodName.
	(c ~= Undefined) ifTrue: [
		'Compiling ' print. c print. ':' print.
		name print. '... ' print
	].
	encoder name: name.
	self readMethodVariables.
	self readBody compile: encoder block: false.
	meth <- encoder method: maxTemps class: c text: text.
	meth args: argNames inst: instNames temp: tempNames.
	^ meth
!
METHOD Parser
parse: c
	^ self parse: c with: Encoder
!
METHOD Parser
error: aString
	'Compile error near line ' print.
	lineNum printString print.
	': ' print.
	aString printNl.
	errBlock value
!
METHOD Parser
currentChar
	^ text at: index ifAbsent: [ Char eof ]
!
METHOD Parser
nextChar
	(self currentChar = Char newline) ifTrue: [
		lineNum <- lineNum + 1
	].
	index <- index + 1.
	^ self currentChar
!
METHOD Parser
nextLex	
	self skipBlanks.
	tokenType <- self currentChar.
	tokenType isEOF   " end of input "
		ifTrue: [ tokenType <- $  . token <- nil. ^ nil ].
	tokenType isDigit ifTrue: [ ^ self lexInteger ].
	tokenType isAlphabetic ifTrue: [ ^ self lexAlnum ].
	^ self lexBinary
!
METHOD Parser
skipBlanks  | cc |
	cc <- self currentChar.
	[ cc isBlank ] whileTrue: [ cc <- self nextChar ].
	( cc = $" ) ifTrue: [ self skipComment ]
!
METHOD Parser
skipComment  | cc |
	[ cc <- self nextChar.
	  cc isEOF ifTrue: [ ^ self error: 'unterminated comment'].
	  cc ~= $" ] whileTrue: [ nil ].
	self nextChar. self skipBlanks
!
METHOD Parser
lexInteger	| start |
	start <- index.
	[ self nextChar isDigit ]
		whileTrue: [ nil ].
	token <- text from: start to: index - 1
!
METHOD Parser
lexAlnum | cc start |
	start <- index.
	[ ((cc <- self nextChar) isAlphanumeric) or: [ cc = $: ] ]
			whileTrue: [ nil ].
		" add any trailing colons "
	token <- text from: start to: index - 1
!
METHOD Parser
lexBinary	| c d |
	c <- self currentChar.
	token <- c asString.
	d <- self nextChar.
	(self charIsSyntax: c) ifTrue: [ ^ token ].
	(((d isBlank
		or: [ d isDigit])
		or: [ d isAlphabetic ])
		or: [ self charIsSyntax: d])
			ifTrue: [ ^ token ].
	token <- token + d asString.
	self nextChar
!
METHOD Parser
charIsSyntax: c
	^ ('.()[]#^$;' includes: c) or: [ c = $' ]
!
METHOD Parser
readMethodName   | name |
	self tokenIsName	" unary method "
		ifTrue: [ name <- token. self nextLex. ^ name ].
	self tokenIsBinary	" binary method "
		ifTrue: [ name <- token. self nextLex.
			self tokenIsName
				ifFalse: [ self error: 'missing argument'].
				self addArgName: token asSymbol.
				self nextLex. ^ name ].
	self tokenIsKeyword
		ifFalse: [ self error: 'invalid method header'].
	name <- ''.
	[ self tokenIsKeyword ]
		whileTrue: [ name <- name + token. self nextLex.
			self tokenIsName
				ifFalse: [ self error: 'missing argument'].
				self addArgName: token asSymbol.
				self nextLex ].
	^ name
!
METHOD Parser
addArgName: name
	((instNames includes: name)
		or: [ argNames includes: name ])
		ifTrue: [ self error: 'doubly defined argument name: ' +
			name asString].
	argNames <- argNames with: name
!
METHOD Parser
tokenIsName
	tokenType isAlphabetic ifFalse: [ ^ false ].
	^ (token at: token size) isAlphanumeric
!
METHOD Parser
tokenIsKeyword
	tokenType isAlphabetic ifFalse: [ ^ false ].
	^ (token at: token size) = $:
!
METHOD Parser
tokenIsBinary
	(((token isNil
		or: [ self tokenIsName])
		or: [ self tokenIsKeyword])
		or: [ self charIsSyntax: tokenType ]) ifTrue: [ ^ false ].
	^ true
!
METHOD Parser
readMethodVariables
	tokenType = $| ifFalse: [ ^ nil ].
	self nextLex.
	[ self tokenIsName ]
		whileTrue: [ self addTempName: token asSymbol. self nextLex ].
	tokenType = $|
		ifTrue: [ self nextLex ]
		ifFalse: [ self error: 'illegal method variable declaration']
!
METHOD Parser
addTempName: name
	(((argNames includes: name)
		or: [ instNames includes: name ] )
		or: [ tempNames includes: name ] )
		ifTrue: [ self error: 'doubly defined name '].
	tempNames <- tempNames with: name.
	maxTemps <- maxTemps max: tempNames size
!
METHOD Parser
readBody | lnum |
	lnum <- lineNum.
	^ (BodyNode at: lnum) statements: self readStatementList
!
METHOD Parser
readStatementList   | list |
	list <- List new.
	[ list add: self readStatement.
	  tokenType notNil and: [ tokenType = $. ] ]
		whileTrue: [ self nextLex.
			(token isNil or: [ tokenType = $] ] )
				ifTrue: [ ^ list ] ].
	^ list
!
METHOD Parser
readStatement | lnum |
	tokenType = $^
		ifTrue: [ lnum <- lineNum. self nextLex.
			^ (ReturnNode at: lnum)
				expression: self readExpression ].
	^ self readExpression
!
METHOD Parser
readExpression   | node lnum |
	self tokenIsName ifFalse: [ ^ self readCascade: self readTerm ].
	node <- self nameNode: token asSymbol. self nextLex.
	self tokenIsArrow
		ifTrue: [ node assignable
				ifFalse: [ self error: 'illegal assignment'].
			lnum <- lineNum.
			self nextLex.
			^ (AssignNode at: lnum) target:
				node expression: self readExpression ].
	^ self readCascade: node
!
METHOD Parser
tokenIsArrow
	(token isKindOf: String) ifFalse: [ ^ false ].
	^ token = '<-'
!
METHOD Parser
readTerm   | node lnum |
	token isNil
		ifTrue: [ self error: 'unexpected end of input' ].
	tokenType = $(
		ifTrue: [ self nextLex. node <- self readExpression.
			tokenType = $)
				ifFalse: [ self error: 'unbalanced parenthesis' ].
			self nextLex. ^ node ].
	tokenType = $[ ifTrue: [ ^ self readBlock ].
	tokenType = $< ifTrue: [ ^ self readPrimitive ].
	self tokenIsName
		ifTrue: [ node <- self nameNode: token asSymbol.
			self nextLex. ^ node ].
	lnum <- lineNum.
	^ (LiteralNode at: lnum) value: self readLiteral
!
METHOD Parser
nameNode: name
	" make a new name node "
	name == #super
		ifTrue: [ ^ (ArgumentNode at: lineNum) position: 0 ].
	(1 to: tempNames size) do: [:i |
		(name == (tempNames at: i))
			ifTrue: [ ^ (TemporaryNode at: lineNum)
				position: i ] ].
	(1 to: argNames size) do: [:i |
		(name == (argNames at: i))
			ifTrue: [ ^ (ArgumentNode at: lineNum) position: i ] ].
	(1 to: instNames size) do: [:i |
		(name == (instNames at: i))
			ifTrue: [ ^ (InstNode at: lineNum) position: i ] ].
	^ (LiteralNode at: lineNum);
		value: (globals at: name
			ifAbsent: [ ^ self error:
				'unrecognized name: ' + name printString ])
!
METHOD Parser
readLiteral   | node |
	tokenType = $$
		ifTrue: [ node <- self currentChar.
			self nextChar. self nextLex. ^ node ].
	tokenType isDigit
		ifTrue: [ ^ self readInteger ].
	token = '-'
		ifTrue: [ self nextLex. ^ self readInteger negated ].
	tokenType = $'
		ifTrue: [ ^ self readString ].
	tokenType = $#
		ifTrue: [ ^ self readSymbol ].
	self error: 'invalid literal: ' + token
!
METHOD Parser
readInteger  | value |
	value <- token asNumber.
	value isNil ifTrue: [ self error: 'integer expected' ].
	self nextLex.
	^ value
!
METHOD Parser
readString  | first last cc |
	first <- index.
	[ cc <- self currentChar.
	  cc isNil ifTrue: [ self error: 'unterminated string constant'].
	  cc ~= $' ] whileTrue: [ index <- index + 1 ].
	last <- index - 1.
	self nextChar = $'
		ifTrue: [ self nextChar.
			^ (text from: first to: index - 2) + self readString ].
	self nextLex.
	^ text from: first to: last
!
METHOD Parser
readSymbol   | cc |
	cc <- self currentChar.
	(cc isEOF or: [ cc isBlank])
		ifTrue: [ self error: 'invalid symbol'].
	cc = $( ifTrue: [ ^ self readArray ].
	(self charIsSyntax: cc)
		ifTrue: [ self error: 'invalid symbol'].
	self nextLex.
	cc <- Symbol new: token. self nextLex.
	^ cc
!
METHOD Parser
readArray	| value |
	self nextChar. self nextLex. value <- Array new: 0.
	[ tokenType ~= $) ]
		whileTrue: [ value <- value with: self arrayLiteral ].
	self nextLex.
	^ value
!
METHOD Parser
arrayLiteral	| node |
	tokenType isAlphabetic
		ifTrue: [ node <- Symbol new: token. self nextLex. ^ node ].
	^ self readLiteral
!
METHOD Parser
readPrimitive  | num args lnum |
	lnum <- lineNum.
	self nextLex.
	num <- self readInteger.
	args <- List new.
	[ tokenType ~= $> ]
		whileTrue: [ args add: self readTerm ].
	self nextLex.
	^ (PrimitiveNode at: lnum) number: num arguments: args
!
METHOD Parser
readBlock    | stmts saveTemps lnum |
	saveTemps <- tempNames.
	lnum <- lineNum.
	self nextLex.
	tokenType = $:
		ifTrue: [ self readBlockTemporaries ].
	stmts <- self readStatementList.
	tempNames <- saveTemps.
	tokenType = $]
		ifTrue: [ self nextLex.
			^ (BlockNode at: lnum) statements: stmts
				temporaryLocation: saveTemps size ]
		ifFalse: [ self error: 'unterminated block']
!
METHOD Parser
readBlockTemporaries
	[ tokenType = $: ]
		whileTrue: [ self currentChar isAlphabetic
			ifFalse: [ self error: 'ill formed block argument'].
			self nextLex.
			self tokenIsName
				ifTrue: [ self addTempName: token asSymbol ]
				ifFalse: [ self error: 'invalid block argument list '].
			self nextLex ].
	tokenType = $|
		ifTrue: [ self nextLex ]
		ifFalse: [ self error: 'invalid block argument list ']
!
METHOD Parser
readCascade: base   | node list |
	node <- self keywordContinuation: base.
	tokenType = $;
		ifTrue: [ node <- (CascadeNode at: lineNum) head: node.
			list <- List new.
			[ tokenType = $; ]
				whileTrue: [ self nextLex.
					list add:
						(self keywordContinuation: nil ) ].
			node list: list ].
	^ node
!
METHOD Parser
keywordContinuation: base  | receiver name args lnum |
	receiver <- self binaryContinuation: base.
	self tokenIsKeyword
		ifFalse: [ ^ receiver ].
	name <- ''.
	args <- List new.
	lnum <- lineNum.
	[ self tokenIsKeyword ]
		whileTrue: [ name <- name + token. self nextLex.
			args add:
				(self binaryContinuation: self readTerm) ].
	^ (MessageNode at: lnum) receiver:
		receiver name: name asSymbol arguments: args
!
METHOD Parser
binaryContinuation: base | receiver name lnum |
	receiver <- self unaryContinuation: base.
	[ self tokenIsBinary]
		whileTrue: [ lnum <- lineNum.
			name <- token asSymbol. self nextLex.
			receiver <- (MessageNode at: lnum)
				receiver: receiver name: name arguments:
					(List with:
						(self unaryContinuation: self readTerm)) ].
	^ receiver
!
METHOD Parser
unaryContinuation: base | receiver lnum |
	receiver <- base.
	[ self tokenIsName ]
		whileTrue: [ lnum <- lineNum.
			receiver <- (MessageNode at: lnum)
				receiver: receiver name: token asSymbol
					arguments: (List new).
				self nextLex ].
	^ receiver
!
METHOD MetaParserNode
new
	self error: 'Must use at: for creation'
!
METHOD MetaParserNode
at: l | ret |
	ret <- super new.
	self in: ret at: 1 put: l.
	^ ret
!
METHOD ParserNode
isSuper
	^ false
!
METHOD ParserNode
isBlock
	^ false
!
METHOD ParserNode
assignable
	^ false
!
METHOD ParserNode
compile: encoder
	encoder lineNum: lineNum
!
METHOD BodyNode
statements: s
	statements <- s
!
METHOD BodyNode
compile: encoder block: inBlock
	super compile: encoder.
	statements reverseDo:
		[ :stmt | stmt compile: encoder block: inBlock.
			encoder genHigh: 15 low: 5 " pop "].
	encoder genHigh: 15 low: 1 " return self "
!
METHOD ReturnNode
expression: e
	expression <- e
!
METHOD ReturnNode
compile: encoder block: inBlock
	super compile: encoder.
	expression compile: encoder block: inBlock.
	inBlock
		ifTrue: [ encoder genHigh: 15 low: 3 " block return " ]
		ifFalse: [ encoder genHigh: 15 low: 2 " stack return " ]
!
METHOD AssignNode
target: t expression: e
	target <- t.
	expression <- e
!
METHOD AssignNode
compile: encoder block: inBlock
	super compile: encoder.
	expression compile: encoder block: inBlock.
	target assign: encoder
!
METHOD LiteralNode
value: v
	value <- v
!
METHOD LiteralNode
compile: encoder block: inBlock
	super compile: encoder.
	value == nil ifTrue: [ ^ encoder genHigh: 5 low: 10 ].
	value == true ifTrue: [ ^ encoder genHigh: 5 low: 11 ].
	value == false ifTrue: [ ^ encoder genHigh: 5 low: 12 ].
	(((value class == SmallInt) and:
	 [value < 10]) and: [value negative not])
		ifTrue: [ ^ encoder genHigh: 5 low: value ].
	encoder genHigh: 4 low: (encoder genLiteral: value)
!
METHOD ArgumentNode
position: p
	position <- p
!
METHOD ArgumentNode
isSuper
	^ position = 0
!
METHOD ArgumentNode
compile: encoder block: inBlock
	super compile: encoder.
	position = 0
		ifTrue: [ encoder genHigh: 2 low: 0 ]
		ifFalse: [ encoder genHigh: 2 low: position - 1 ]
!
METHOD TemporaryNode
position: p
	position <- p
!
METHOD TemporaryNode
compile: encoder block: inBlock
	super compile: encoder.
	encoder genHigh: 3 low: position - 1
!
METHOD TemporaryNode
assignable
	^ true
!
METHOD TemporaryNode
assign: encoder
	encoder genHigh: 7 low: position - 1
!
METHOD InstNode
position: p
	position <- p
!
METHOD InstNode
compile: encoder block: inBlock
	super compile: encoder.
	encoder genHigh: 1 low: position - 1
!
METHOD InstNode
assign: encoder
	encoder genHigh: 6 low: position - 1
!
METHOD InstNode
assignable
	^ true
!
METHOD PrimitiveNode
number: n arguments: a
	number <- n.
	arguments <- a.
!
METHOD PrimitiveNode
compile: encoder block: inBlock | argsize |
	argsize <- arguments size.
	super compile: encoder.
	encoder pushArgs: argsize.
	arguments reverseDo: [ :a | a compile: encoder block: inBlock ].
	encoder genHigh: 13 low: argsize.
	encoder genCode: number.
	encoder popArgs: argsize
!
METHOD BlockNode
statements: s temporaryLocation: t
	statements <- s.
	temporaryLocation <- t
!
METHOD BlockNode
compileInLine: encoder block: inBlock
	statements reverseDo:
		[ :stmt | stmt compile: encoder block: inBlock.
			encoder genHigh: 15 low: 5 " pop top " ].
	encoder backUp
!
METHOD BlockNode
isBlock
	^ true
!
METHOD BlockNode
compile: encoder block: inBlock | patchLocation |
	super compile: encoder.
	encoder genHigh: 12 low: temporaryLocation.
	patchLocation <- encoder genVal: 0.
	self compileInLine: encoder block: true.
	encoder genHigh: 15 low: 2. " return top of stack "
	encoder patch: patchLocation
!
METHOD CascadeNode
head: h
	head <- h
!
METHOD CascadeNode
list: l
	list <- l
!
METHOD CascadeNode
compile: encoder block: inBlock
	super compile: encoder.
	head compile: encoder block: inBlock.
	list reverseDo: [ :stmt |
		encoder genHigh: 15 low: 4. " duplicate "
		stmt compile: encoder block: inBlock.
		encoder genHigh: 15 low: 5 "pop from stack " ]
!
METHOD MessageNode
receiver: r name: n arguments: a
	receiver <- r.
	name <- n.
	arguments <- a
!
METHOD MessageNode
compile: encoder block: inBlock
	super compile: encoder.
	receiver isNil
		ifTrue: [ ^ self cascade: encoder block: inBlock ].
	((receiver isBlock and: [ self argumentsAreBlock ])
		and: [name = #whileTrue: or: [ name = #whileFalse ] ] )
		ifTrue: [ ^ self optimizeWhile: encoder block: inBlock ].
	receiver compile: encoder block: inBlock.
	receiver isSuper
		ifTrue: [ ^ self sendToSuper: encoder block: inBlock ].
	name = #isNil ifTrue: [ ^ encoder genHigh: 10 low: 0 ].
	name = #notNil ifTrue: [ ^ encoder genHigh: 10 low: 1 ].
	self compile2: encoder block: inBlock
!
METHOD MessageNode
compile2: encoder block: inBlock
	self argumentsAreBlock ifTrue: [
		name = #ifTrue: ifTrue: [ ^ self compile: encoder
				test: 8 constant: 10 block: inBlock ].
		name = #ifFalse: ifTrue: [ ^ self compile: encoder
				test: 7 constant: 10 block: inBlock ].
		name = #and: ifTrue: [ ^ self compile: encoder
				test: 8 constant: 12 block: inBlock ].
		name = #or: ifTrue: [ ^ self compile: encoder
				test: 7 constant: 11 block: inBlock ].
		name = #ifTrue:ifFalse:
			ifTrue: [ ^ self optimizeIf: encoder block: inBlock ].
		].
	self evaluateArguments: encoder block: inBlock.
	name = '<' asSymbol ifTrue: [ ^ encoder genHigh: 11 low: 0].
	name = '<=' asSymbol ifTrue: [ ^ encoder genHigh: 11 low: 1].
	name = '+' asSymbol ifTrue: [ ^ encoder genHigh: 11 low: 2].
	self sendMessage: encoder block: inBlock
!
METHOD MessageNode
sendToSuper: encoder block: inBlock
	self evaluateArguments: encoder block: inBlock.
	encoder genHigh: 8 low: 1 + arguments size.
	encoder genHigh: 15 low: 11.
	encoder genCode: (encoder genLiteral: name)
!
METHOD MessageNode
cascade: encoder block: inBlock
	self evaluateArguments: encoder block: inBlock.
	self sendMessage: encoder block: inBlock
!
METHOD MessageNode
evaluateArguments: encoder block: inBlock
	encoder pushArgs: 1 + arguments size.
	arguments reverseDo: [ :arg |
		arg compile: encoder block: inBlock ]
!
METHOD MessageNode
sendMessage: encoder block: inBlock
	encoder popArgs: arguments size.
		" mark arguments, then send message "
	encoder genHigh: 8 low: 1 + arguments size.
	encoder genHigh: 9 low: (encoder genLiteral: name)
!
METHOD MessageNode
argumentsAreBlock
	arguments do: [ :arg | arg isBlock ifFalse: [ ^ false ]].
	^ true
!
METHOD MessageNode
optimizeWhile: encoder block: inBlock | start save |
	start <- encoder currentLocation.
	receiver compileInLine: encoder block: inBlock.
	name = #whileTrue:	" branch if false/true "
		ifTrue: [ encoder genHigh: 15 low: 8 ]
		ifFalse: [ encoder genHigh: 15 low: 7 ].
	save <- encoder genVal: 0.
	arguments first compileInLine: encoder block: inBlock.
	encoder genHigh: 15 low: 5. " pop from stack "
	encoder genHigh: 15 low: 6. " branch "
	encoder genVal: start. " branch target "
	encoder patch: save.
	encoder genHigh: 5 low: 10  " push nil "
!
METHOD MessageNode
compile: encoder test: t constant: c block: inBlock | save ssave |
	super compile: encoder.
	encoder genHigh: 15 low: t.  " branch test "
	save <- encoder genVal: 0.
	arguments first compileInLine: encoder block: inBlock.
	encoder genHigh: 15 low: 6.  " branch "
	ssave <- encoder genVal: 0.
	encoder patch: save.
	encoder genHigh: 5 low: c.  " push constant "
	encoder patch: ssave
!
METHOD MessageNode
optimizeIf: encoder block: inBlock | save ssave |
	encoder genHigh: 15 low: 7.  " branch if true test "
	save <- encoder genVal: 0.
	arguments first compileInLine: encoder block: inBlock.
	arguments removeFirst.
	encoder genHigh: 15 low: 6.  " branch "
	ssave <- encoder genVal: 0.
	encoder patch: save.
	arguments first compileInLine: encoder block: inBlock.
	encoder patch: ssave
!
METHOD Encoder
name: n
	name <- n asSymbol.
	byteCodes <- ByteArray new: 20.
	index <- 0.
	literals <- Array new: 0.
	stackSize <- 0.
	maxStack <- 1.
!
METHOD Encoder
lineNum: l
	" Don't care, except in DebugEncoder subclass "
!
METHOD Encoder
pushArgs: n
	stackSize <- stackSize + n.
	maxStack <- stackSize max: maxStack
!
METHOD Encoder
popArgs: n
	stackSize <- stackSize - n.
!
METHOD Encoder
genLiteral: aValue | idx |
	idx <- literals indexOf: aValue.
	idx notNil ifTrue: [ ^ idx - 1 ].
	literals <- literals with: aValue.
	^ literals size - 1
!
METHOD Encoder
genHigh: high low: low
	(low >= 16)
		ifTrue: [ self genHigh: 0 low: high. self genCode: low ]
		ifFalse: [ self genCode: high * 16 + low ]
!
METHOD Encoder
genCode: byte
	index <- index + 1.
	(index >= byteCodes size)
		ifTrue: [ self expandByteCodes].
	byteCodes at: index put: byte.
	^ index
!
METHOD Encoder
genVal: byte
	self genCode: (byte rem: 256).
	self genCode: (byte quo: 256).
	^ index-1
!
METHOD Encoder
expandByteCodes	| newarray size |
	size <- byteCodes size.
	newarray <- ByteArray new: size + 8.
	1 to: size do: [:i | newarray at: i put: (byteCodes at: i)].
	byteCodes <- newarray
!
METHOD Encoder
patch: loc
		" patch a goto from a block "
	byteCodes at: loc put: (index rem: 256).
	byteCodes at: (loc + 1) put: (index quo: 256)
!
METHOD Encoder
currentLocation
	^ index
!
METHOD Encoder
backUp
	" back up one instruction "
	index <- index - 1
!
METHOD Encoder
method: maxTemps class: c text: text
	^ Method name: name byteCodes: byteCodes literals: literals
		stackSize: maxStack temporarySize: maxTemps class: c
		text: text
!
BEGIN nil main
END

```

```st lst4/src/ImageBuilder/smalltalk/Tests.st

# This is a shell archive.  Remove anything before this line,
# then unpack it by saving it in a file and typing "sh file".
#
# Wrapped by Tim Budd <budd@ada> on Fri Dec 16 10:16:47 1994
#
# This archive contains:
#	concordance.stt	palendrome.stt	queen.stt	test1.stt	
#	while.stt	
#

LANG=""; export LANG
PATH=/bin:/usr/bin:$PATH; export PATH

echo x - concordance.stt
cat >concordance.stt <<'@EOF'
/  Concordance program
+Object subclass: #Concordance variables: #(dict)
!Concordance
word: word occursOnLine: line
	(dict includes: word)
		ifFalse: [ dict at: word put: List new ].
	((dict at: word) includes: line)
		ifFalse: [ (dict at: word) addLast: line]
!
!Concordance
fileIn	| text lineNumber words |
	lineNumber <- 0.
	[ text <- String input.  text notNil ]
		whileTrue: [ 
			text <- text collect: [ :c | c lowerCase ].
			lineNumber <- lineNumber + 1.
			words <- text break: ' .,'.
			words do: [ :word | self word: word 
				occursOnLine: lineNumber ] ]
!
!Concordance
displayDictionary
	dict binaryDo: [ :word :lines |
		word print.
		lines do: [ :line | ' ' print. line print ].
		Char newline print ]
!
!Concordance
run
	dict <- Dictionary new.
	self fileIn.
	self displayDictionary
!
+'enter test lines, followed by end-of-file'
+Concordance new run
/ end
@EOF

chmod 600 concordance.stt

echo x - palendrome.stt
cat >palendrome.stt <<'@EOF'
/ A simple palendrome tester
/ first, add a method to class String
!String
isPalendrome | string |
	string <- self select: [:c | c isAlphabetic ].
	string <- string collect: [:c | c lowerCase ].
	^ string = string reverse
!
/ next, add a new class with a few test cases
+Object subclass: #Pal
!Pal
test: aString
	aString print.
	' : ' print.
	aString isPalendrome print.
	Char newline print.
!
!Pal
runTests
	self test: 'A Man, A Plan, A Canal, Panama'.
	self test: 'Rats live on no Evil star'.
	self test: 'This is not a palendrome'
!
+Pal new runTests
/ end of file
@EOF

chmod 600 palendrome.stt

echo x - queen.stt
cat >queen.stt <<'@EOF'
/ eight queens exercise
+Object subclass: #NullQueen 
!NullQueen
first
	^ true
!
!NullQueen
next
	^ false
!
!NullQueen
checkRow: row column: column
	^ false
!
!NullQueen
result
	^ List new
!
+Object subclass: #Queen variables: #(row column neighbor)
!Queen
setColumn: aNumber neighbor: aQueen
	column <- aNumber.
	neighbor <- aQueen
!
!Queen
checkRow: testRow column: testColumn | columnDifference |
	columnDifference <- testColumn - column.
	(((row = testRow) or:
		[ row + columnDifference = testRow]) or:
		[ row - columnDifference = testRow])
			ifTrue: [ ^ true ].
	^ neighbor checkRow: testRow column: testColumn
!
!Queen
first
	neighbor first.
	row <- 1.
	^ self testPosition
!
!Queen
next
	^ self advance and: [ self testPosition ]
!
!Queen
advance
	(row = 8)
		ifTrue: [ neighbor next ifFalse: [ ^ false ].
			row <- 0 ].
	row <- row + 1.
	^ true
!
!Queen
testPosition
	[ neighbor checkRow: row column: column ]
		whileTrue: [ self advance ifFalse: [ ^ false ]].
	^ true
!
!Queen
result	
	^ neighbor result; addLast: row
!
+Object subclass: #Main
!Main
run	| lastQueen |
	lastQueen <- NullQueen new.
	1 to: 8 do: [:i | lastQueen <- Queen new; 
		setColumn: i neighbor: lastQueen ].
	lastQueen first.
	'got a result' print.
	lastQueen result do: [:x | x print. ' ' print ].
	Char newline print.
	[ lastQueen next ] whileTrue:
		[
	lastQueen result do: [:x | x print. ' ' print ].
	Char newline print ]
!
+Main new run
/end
@EOF

chmod 600 queen.stt

echo x - test1.stt
cat >test1.stt <<'@EOF'
/ test class instance variables
+Object subclass: #A variables: #( #a )
+A subclass: #B variables: #( #b )
!A
seta: x
	a <- x
!
!A
geta
   ^ a
!
!B
setb: y
   b <- y.
   a <- y + 1.
!
!B
getb
   ^ b
!
!B
test
   self seta: 3.
   self geta print.
   self setb: 7.
   self geta print.
   self getb print.
   Char newline print
!
+B new test
@EOF

chmod 600 test1.stt

echo x - while.stt
cat >while.stt <<'@EOF'
!Object
isKindOf: aClass	| clas |
	clas <- self class.
	[ clas notNil ] whileTrue:
		[ clas == aClass ifTrue: [ ^ true ].
		  clas <- clas superclass ].
	^ false
!
!Undefined
main	| command |
	[ '-> ' print. command <- String input. command notNil ]
		whileTrue: [ command isEmpty
			ifFalse: [ command doIt print. Char newline print ] ]
!
=String
input	| value c nl |
	" read a line from input "
	value <- ''. nl <- Char newline.
	[ c <- Char input. 
	  c isNil ifTrue: [ ^ nil ]. c ~= nl ] whileTrue:
		[ value <- value + c asString ].
	^ value
!
!Number
to: limit do: aBlock  | i |
		" optimize arithmetic loops "
	i <- self.
	[ i <= limit ] whileTrue: [ aBlock value: i. i <- i + 1 ]
!
!File
readLine	| value  c|
	" read a line from input "
	value <- ''.
	[ c <- self readChar. 
	  c isNil ifTrue: [ ^ nil ]. c ~= Char newline ] whileTrue:
		[ value <- value + c asString ].
	^ value
!
!Parser
skipBlanks  | cc |
	[ cc <- self currentChar.
	  cc notNil and: [ cc isBlank ] ]
			whileTrue: [ index <- index + 1 ].
	(cc notNil and: [ cc = $" ] )
		ifTrue: [ self skipComment ]
!
!Parser
skipComment  | cc |
	[ index <- index + 1.
	  cc <- self currentChar.
	  cc isNil 
		ifTrue: [ ^ self error: 'unterminated comment'].
	  cc ~= $" ] whileTrue: [ nil ].
	self nextChar. self skipBlanks
!
+1 to: 15 do: [:x| x factorial print. Char newline print]
+(5 to: 1 by: -1) do: [:x| x printNl]
/ end of input
@EOF

chmod 600 while.stt

exit 0

```

```st lst4/src/ImageBuilder/smalltalk/browser.st
" browser.st
	Utility to interactively examine data structures
"
+Object subclass: #Browser variables: #(stack)

" on:
	Instantiate a browser on the named object
"
=Browser
on: obj
	self new on: obj
!

" showPos
	Display stack of objects we're inspecting
"
!Browser
showPos | cnt |
	' == stack -> ' print.
	cnt <- 0.
	stack do: [:obj|
		(cnt > 0) ifTrue: [ ' / ' print ].
		obj print.
		cnt <- cnt + 1
	].
	'' printNl
!

" ivars
	Dump instance variables of named object
"
!Browser
ivars: obj | iv |
	iv <- obj class instanceVariables.
	1 to: iv size do: [:idx|
		idx print. Char tab print.
		(iv at: idx) print. ': ' print.
		(Object in: obj at: idx) printNl
	]
!

" run:
	Compile and run Smalltalk expression on object
"
!Browser
run: args on: obj | t cl |
	" Assemble arguments into single String "
	args do: [:arg|
		t isNil ifTrue: [ t <- arg ]
		 ifFalse: [ t <- t + ' ' + arg ]
	].

	" Compile into xxBrowse Method for object's class "
	cl <- obj class.
	t <- 'xxBrowse' + String newline + String tab + '^ ' + t.
	(cl addMethod: t) notNil ifTrue: [
		" Compile OK, run and print result "
		obj xxBrowse printNl.
		" Now clean up (remove) browse method "
		cl removeMethod: #xxBrowse
	]
!

" on:
	Main UI loop for browser
"
!Browser
on: top | cmd done last obj args idx |
	stack <- Array with: top.
	[true] whileTrue: [
		" Show where we are and get next command "
		self showPos.

		" Get next command, explode into cmd/args "
		'Browse> ' print.
		cmd <- String input.
		(cmd isNil) ifTrue: [ ^ nil ].
		args <- cmd break: ' '.
		cmd <- args at: 1.
		args <- args from: 2.

		" Get top of stack in its own variable "
		obj <- stack at: (stack size).
		done <- false.

		" Index with at: "
		((cmd = 'i') or: [cmd = 'index']) ifTrue: [
			last <- stack at: (stack size).
			idx <- (args at: 1) asNumber.
			last <- last at: idx.
			last printNl.
			done <- true
		].

		" Nest to last displayed object "
		((cmd = 'n') or: [cmd = 'nest']) ifTrue: [
			stack <- stack with: last.
			done <- true
		].

		" Show class "
		((cmd = 'c') or: [cmd = 'class']) ifTrue: [
			obj class printNl.
			done <- true
		].

		" Pop up a stack level "
		((cmd = 'u') or: [cmd = 'up']) ifTrue: [
			(stack size < 2) ifTrue: [
				'Stack is empty' printNl
			] ifFalse: [
				stack <- stack from: 1 to: (stack size - 1)
			].
			done <- true
		].

		" Raw index "
		(cmd = 'ri') ifTrue: [
			idx <- (args at: 1) asNumber.
			last <- Object in: obj at: idx.
			last printNl.
			done <- true
		].

		" List instance variables "
		((cmd = 'iv') or: [cmd = 'ivars']) ifTrue: [
			self ivars: obj.
			done <- true
		].

		" Show size "
		((cmd = 'bs') or: [cmd = 'basicsize']) ifTrue: [
			obj basicSize printNl.
			done <- true
		].
		((cmd = 'sz') or: [cmd = 'size']) ifTrue: [
			obj size printNl.
			done <- true
		].

		" Print arbitrary expression "
		((cmd = 'p') or: [cmd = 'print']) ifTrue: [
			self run: args on: obj.
			done <- true
		].

		" All done "
		((cmd = 'q') or: [cmd = 'quit']) ifTrue: [
			^ nil
		].

		" Unknown command? "
		done ifFalse: [ ('Unknown command: ' + cmd) printNl ]
	]
!

```

```st lst4/src/ImageBuilder/smalltalk/debug.st
"debug.st - a simple interactive debugger for Tiny Smalltalk
 by Andy Valencia, May 2001

 To use the debugger, fileIn: this file.  Then do:

 Debug run: 'command...'

 The given command line will be compiled (much as it would if you had
 typed it interactively).  Then the debugger will be started on this
 command itself.  Generally, you'll do a 's' (or 'step') to step down
 from the command line into the first method call.

 Command available are:
 's' or 'step'		Single step to the next line
 'stepi'		Single step one VM instruction
 'n' or 'next'		Step over method calls on the current line
 'b' or 'break'		Argument is <class>/<method>[/<line #>]
 			Set a breakpoint at the named method.
			Meta<class> accesses class methods.
			A plain line number applies to the current
			 class/method.
 'c' or 'cont'		Continue running until error, completion,
 			or breakpoint.
 'd' or 'delete'	Argument is an integer; delete that breakpoint
 'lb' or 'listbreak'	List breakpoints
 'p' or 'print'		Print variable(s).  Arguments are one or more
 			temporary, argument, or instance variables.
			You may also use $pc (VM instruction pointer)
			and $sp (VM stack top pointer)
 'quit'			Leave the debugger (abandon the executing
 			target code)
 'where' or 'bt'	Show stack backtrace
 'l' or 'list'		List source (can provide a line # as argument)
 'whatis'		Describe variable more thoroughly
 'up', 'down'		Move up and down the call stack for purposes
 			of accessing variables.
 'debug'		Argument is a Class... compiles all Method's
 			for that class in their debuggable form.
 'br' or 'browse'	Argument is variable as in 'print'; invokes
			system data structure browser.
 (blank line)		Re-enter previous command.  Useful for single
 			stepping statements in a row.
"
+Encoder subclass: #DebugEncoder variables: #(lines oldLine oldRange)
+Method subclass: #DebugMethod variables: #(lines textlines vars bpoints active)
+Object subclass: #Debug variables: #(proc bpoints prevList selctx)
!DebugEncoder
name: n
	" Hook to set up our instance variables "
	lines <- Dictionary new.
	oldLine <- 0.
	^ super name: n
!
!DebugEncoder
genCode: byte
	" Record code generated versus line number "
	oldRange isNil ifTrue: [
		oldRange <- lines at: oldLine ifAbsent: [nil].
		oldRange isNil ifTrue: [
			oldRange <- Set new.
			lines at: oldLine put: oldRange
		]
	].
	oldRange add: index.
	^ super genCode: byte
!
!DebugEncoder
lineNum: l
	" Note when the line number changes "
	(l ~= oldLine) ifTrue: [
		oldLine <- l.
		" We lazily insert the range, only at the point we
		  actually see some code generated for this source line."
		oldRange <- nil
	]
!
!DebugEncoder
method: maxTemps class: c text: text | ret |
	" Create the compiled Method from the generated code "
	ret <- DebugMethod name: name byteCodes: byteCodes
		literals: literals stackSize: maxStack
		temporarySize: maxTemps class: c text: text.
	ret debug: lines.
	^ ret
!
!Class
setDebug: sel | meth cl dict |
	" Recompile a Method into a DebugMethod, recording additional
	  debug information. "

	" Chase up the superclass chain, trying to find our Method "
	cl <- self.
	[meth isNil] whileTrue: [
		dict <- cl methods.
		meth <- dict at: sel ifAbsent: [ cl <- cl superclass. nil].
		cl isNil ifTrue: [
			('Undefined method ' + sel printString +
				' for class ' + self printString) printNl.
			^ nil
		]
	].

	" If we haven't already fixed it up, build a DebugMethod version "
	(meth isKindOf: DebugMethod) ifFalse: [
		'Compiling ' print. sel printString print.
		' for class ' print. cl print.
		'...' printNl.
		meth <- (Parser new text: meth text
			instanceVars: self instanceVariables)
			 parse: self with: DebugEncoder.
		meth notNil
		 ifTrue: [
			cl methods at: sel put: meth.
			Method flushCache
		 ]
		 ifFalse: [
			'Compilation failed.' printNl.
		 ]
	].
	^ meth
!
!Debug
runIt: count | ret |
	" Blow away any selected context when we run "
	selctx <- nil.

	" Execute for one instruction.  Return whether or not the return
	 was 'normal' (i.e., VM stopped due to debugger control, not
	 something else.  Spit out a message for a breakpoint. "
	ret <- proc doExecute: count+1.
	(ret = 5) ifTrue: [ ^ true ].
	(ret = 6) ifTrue: [ self onBreak. ^ true ].
	(ret = 2) ifTrue: [ 'Error trap' printNl ].
	(ret = 3) ifTrue: [ 'Message not understood' printNl ].
	(ret = 4) ifTrue: [ 'Method returned' printNl ].
	^ false
!
!Context
method
	^ method
!
!Context
bytePointer
	^ bytePointer
!
!Context
stack
	^ stack
!
!Context
stackTop
	^ stackTop
!
!Context
temporaries
	^ temporaries
!
!Context
arguments
	^ arguments
!
!Method
forClass
	^ class
!
!Debug
srcLine: ctx | meth line |
	" Get source line corresponding to current byte pointer "
	meth <- ctx method.

	" Can't show it if this isn't a debuggable method "
	(meth isKindOf: DebugMethod) ifFalse: [
		'Not debuggable' printNl.
		^ nil
	].

	" Return source line pointer or nil "
	^ meth srcLine: ctx bytePointer.
!
" Show current source line of a given context "
!Debug
showLine: ctx | line meth |
	" Show source line corresponding to current VM instruction "
	ctx isNil ifTrue: [ ^ nil ].
	meth <- ctx method.
	line <- self srcLine: ctx.
	line isNil
		ifTrue: [
			'Method ' print.  meth name print.
			' for class ' print.  meth forClass print.
			': no source displayed.' printNl
		]
		ifFalse: [
			" Show the text "
			(line printString printWidth: 8) print.
			(meth textlines at: line ifAbsent: ['']) printNl
		]
!
" Display current line of active procedure "
!Debug
showLine
	^ self showLine: self curContext
!
=Debug
run: line | meth ret ctx proc |
	" Run a command line under the debugger"

	meth <- (Parser new text: ('debugCmd ^' + line) instanceVars: #())
		 parse: Undefined with: DebugEncoder.
	meth notNil ifTrue: [
		ret <- super new.
		ctx <- Context new.
		ctx setup: meth withArguments: (Array new: 1).
		proc <- Process new.
		proc context: ctx.
		ret proc: proc.
		ret run
	]
!
!Debug
proc: p
	" Initialize our new debug session "
	proc <- p.
	bpoints <- Array new: 0
!
!Debug
atCall | ret meth ctx pc low high |
	" Tell if the VM instruction pointer is at a method invocation "

	" Get the next instruction "
	ctx <- proc context.
	meth <- ctx method.
	pc <- ctx bytePointer.
	high <- meth byteCodes at: (pc + 1) ifAbsent: [ ^ nil ].
	pc <- pc + 1.
	low <- high rem: 16.
	high <- high quo: 16.
	(high = 0) ifTrue: [
		high <- low.
		low <- meth byteCodes at: (pc + 1) ifAbsent: [ ^ nil ].
		pc <- pc + 1
	].

	" Return nil if it isn't a SendMessage "
	(high = 9) ifFalse: [ ^ nil ].

	" Otherwise return the selector and return address "
	ret <- Array new: 2.
	ret at: 1 put: (meth literals at: (low + 1)).
	ret at: 2 put: pc.
	^ ret
!
!Debug
stepCall: sel | ctx stack sp args target meth |
	" Set up to step into a new method "
	ctx <- proc context.
	stack <- ctx stack.
	sp <- ctx stackTop.
	args <- stack at: sp.
	target <- args at: 1.
	meth <- target class setDebug: sel.
	meth isNil ifTrue: [ ^ true ].
	(self runIt: 1) ifFalse: [
		'Execution done in ' print.
		meth name print.
		' of class ' print.
		target class printNl.
		^ true
	].
	^ false
!
!Debug
onBreak | ctx meth rec |
	" Tell if we're at a breakpoint.  As a side effect, display
	  this fact to the user."
	ctx <- proc context.
	ctx isNil ifTrue: [ ^ false ].
	meth <- ctx method.
	1 to: bpoints size do: [:idx|
		rec <- bpoints at: idx.
		(((rec at: 1) = meth) and:
				[(rec at: 2) = (self srcLine: ctx)])
		ifTrue: [
			'Breakpoint ' print. idx print. ' hit in ' print.
			meth name printString print. '/' print.
			(rec at: 2) printNl.
			^ true
		]
	].
	^ false
!
!Debug
overCall: pc | ctx old res meth |
	" Set a breakpoint at the instruction beyond the SendMessage "
	ctx <- proc context.
	meth <- ctx method.

	" If we're within a non-debug Method, just limp forward "
	(meth isKindOf: DebugMethod) ifFalse: [
		'Can''t step over calls in ' print. meth name print.
		', single stepping.' printNl
		self runIt: 1.
		^ false
	].

	" Otherwise break beyond the call "
	meth setBreak: pc.

	" Now let it run until it hits the breakpoint, and clear
	  the breakpoint. "
	self breakActive: true. meth breakActive: true.
	res <- self runIt: -1.
	self breakActive: false.  meth clearBreak: pc.
	res ifTrue: [
		" Should be stopped at the expected location "
		((proc context = ctx) and:
				[ ctx bytePointer = pc ]) ifTrue: [
			^ false
		].

		" Or hit some other breakpoint "
		(self onBreak) ifTrue: [
			^ false
		].

		" Otherwise, what's going on? "
		'Unexpected run completion' printNl.
		^ true
	].

	" Some other error killed us "
	'Execution aborted' printNl.
	^ true
!
!Debug
doStep: intoCalls | srcl ret ctx |
	" Implement a single step, stepping either over or into calls
	  (method invocations) depending on the intoCalls argument. "
	ctx <- proc context.
	srcl <- self srcLine: ctx.
	[(proc context == ctx) and:
			[srcl == (self srcLine: ctx)]] whileTrue: [
		" If dropping into a new method, deal with it "
		ret <- self atCall.
		ret notNil ifTrue: [
			" Stepping into the call "
			intoCalls ifTrue: [
				^ self stepCall: (ret at: 1)
			].

			" Stepping over call "
			(self overCall: (ret at: 2)) ifTrue: [
				^ true
			]
		] ifFalse: [
			" Otherwise run a single VM operation "
			(self runIt: 1) ifFalse: [
				'Execution done at line ' print.
				srcl printString printNl.
				^ true
			]
		]
	].
	^ false
!
!Debug
printReg: reg in: ctx
	" Print a VM register "
	(reg = '$pc') ifTrue: [
		ctx bytePointer print. ^ self
	].
	(reg = '$sp') ifTrue: [
		ctx stackTop print. ^ self
	].
	'Unknown register: ' print. reg print
!
!Debug
curContext
	selctx isNil ifTrue: [ ^ proc context ].
	^ selctx
!
!Debug
whatis: args | ctx meth |
	" Display arguments, temporaries, instance variables, and
	  VM registers. "

	" Get the DebugMethod, which has symbolic information for variables "
	ctx <- self curContext.
	meth <- ctx method.
	(meth isKindOf: DebugMethod) ifFalse: [
		'No debug information for ' print.
		meth name printNl.
		^ nil
	].

	" Walk each variable, printing its value "
	args do: [:var|
		var print. ': ' print.
		((var at: 1) = $$)
		ifTrue: [
			var print. ' is a register variable' printNl.
		] ifFalse: [
			meth whatis: var in: ctx
		]
	]
!
!Debug
examine: args | ctx meth |
	" Display arguments, temporaries, instance variables, and
	  VM registers. "

	" Get the DebugMethod, which has symbolic information for variables "
	ctx <- self curContext.
	meth <- ctx method.
	(meth isKindOf: DebugMethod) ifFalse: [
		'No debug information for ' print.
		meth name printNl.
		^ nil
	].

	" Walk each variable, printing its value "
	args do: [:var|
		var print. ': ' print.
		((var at: 1) = $$)
		ifTrue: [
			self printReg: var in: ctx
		] ifFalse: [
			meth print: var in: ctx
		].
		Char newline print
	]
!
!Debug
setBreak: args | s cl clname meth methname i rec lineNum inClass arg loc |
	" Set a breakpoint "

	" Map straight line # to current class/method "
	arg <- args at: 1.
	((arg at: 1) isDigit) ifTrue: [
		lineNum <- arg asNumber.
		lineNum isNil ifTrue: [
			'Bad line #' print. arg printNl.
			^ nil
		].
		meth <- self curContext method.
		arg <- (meth forClass printString) + '/' +
			(meth name printString) + '/' +
			lineNum printString
	].

	" Parse <class>:<method> "
	s <- arg break: '/'.
	(s size < 2) ifTrue: [
		'Format is <class>/<method>' printNl.
		^ nil
	].

	" Look up in instance methods unless it's Meta<class>,
	  in which case trim the 'Meta' and look up in class
	  methods."
	clname <- s at: 1.
	((clname from: 1 to: 4) = 'Meta') ifTrue: [
		inClass <- true.
		clname <- clname from: 5 to: clname size
	] ifFalse: [
		inClass <- false
	].
	clname <- clname asSymbol.
	methname <- (s at: 2) asSymbol.

	" Parse line number "
	(s size > 2) ifTrue: [
		lineNum <- (s at: 3) asNumber.
		lineNum isNil ifTrue: [
			'Bad line #' print. (s at: 3) printNl.
			^ nil
		]
	] ifFalse: [
		lineNum <- 1
	].

	" Find class "
	cl <- Smalltalk at: clname ifAbsent: [
		('Unknown class: ' + clname printString) printNl.
		^ nil
	].

	" Convert to metaclass if needed "
	inClass ifTrue: [
		cl <- cl class
	].

	" Now get method, in its debuggable format "
	meth <- cl setDebug: methname.
	meth isNil ifTrue: [
		('Unknown method: ' + methname printString) printNl.
		^ nil
	].

	" If it's already set, don't do it again "
	rec <- Array with: meth with: lineNum.
	i <- bpoints indexOfVal: rec.
	i notNil ifTrue: [
		'Already set as breakpoint ' print.
		i printNl.
		^ nil
	].

	" See if we can turn line # into a code location "
	loc <- meth codeLoc: lineNum.
	loc isNil ifTrue: [
		'No code for source line ' print. lineNum printNl.
		^ nil
	].

	" Set & record the breakpoint "
	meth setBreak: loc.
	bpoints <- bpoints with: rec
!
!Debug
clearBreak: args | arg n rec meth lineNum |
	" Delete an existing breakpoint "

	arg <- args at: 1 ifAbsent: ['Missing argument' printNl. ^ nil].
	n <- arg asNumber.
	n isNil ifTrue: [
		('Invalid argument: ' + arg) printNl
	] ifFalse: [
		((n < 1) or: [n > bpoints size]) ifTrue: [
			('No such breakpoint: ' + arg) printNl
		] ifFalse: [
			rec <- bpoints at: n.
			meth <- rec at: 1.
			lineNum <- rec at: 2.
			meth clearBreak: (meth codeLoc: lineNum).
			bpoints <- bpoints removeIndex: n.
			n print. ': deleted' printNl
		]
	]
!
!Debug
listBreak | rec meth lineNum |
	" List breakpoints "

	'Breakpoints:' printNl.
	1 to: bpoints size do: [:x|
		x print. ': ' print.
		rec <- bpoints at: x.
		meth <- rec at: 1.
		lineNum <- rec at: 2.
		meth name printString print. '/' print.
		lineNum printNl
	]
!
!Debug
breakActive: flag | meths |
	" Make all our breakpoints active or inactive, depending
	  on flag's value. "
	meths <- Set new.
	bpoints do: [:rec|
		meths add: (rec at: 1)
	].
	meths do: [:meth| meth breakActive: flag]
!
!Debug
list: args | meth where src ctx |
	" List source code "

	" Get the method we're going to display "
	ctx <- self curContext.
	meth <- ctx method.
	(meth isKindOf: DebugMethod) ifFalse: [
		'No debug information' printNl.
		^ self
	].

	" Either continue listing, or start from the given place "
	(args size < 1) ifTrue: [
		prevList isNil ifTrue: [
			" List around where we're currently executing "
			where <- (self srcLine: ctx) - 5
		] ifFalse: [
			where <- prevList + 1
		]
	] ifFalse: [
		where <- (args at: 1) asNumber.
		where isNil ifTrue: [
			'Invalid line number: ' print.
			(args at: 1) printNl.
			^ self
		]
	].

	" Show 9 lines "
	src <- meth textlines.
	where to: (where + 8) do: [:x|
		((x > 0) and: [x <= src size]) ifTrue: [
			(x printString printWidth: 8) print.
			(src at: x) printNl.
			prevList <- x
		]
	]
!

" nextContext:
	Return next context deeper in context stack

  Because contexts are only forward linked, we have to search from
  the top inward, then return the next one out.
"
!Debug
nextContext: ctx | c prev |
	c <- proc context.
	[(prev <- c previousContext) ~= ctx] whileTrue: [
		prev isNil ifTrue: [ ^ nil ].
		c <- prev
	].
	^ c
!

" upDown:count:
	Move up or down the stack frames
"
!Debug
upDown: up count: args | c count |
	" If nothing selected, start from bottom of stack "
	selctx <- self curContext.

	" Get count, default 1 "
	(args size > 0) ifTrue: [
		count <- (args at: 1) asNumber
	] ifFalse: [
		count <- 1
	].

	" Walk the context chain "
	1 to: count do: [:ignore|
		" Get next/prev context depending on step direction "
		up ifTrue: [
			c <- selctx previousContext
		] ifFalse: [
			c <- self nextContext: selctx
		].

		" Just ignore running off the end "
		c isNil ifFalse: [ selctx <- c ]
	]
!

" makeDebug:
	Convert Class methods to DebugMethod's
"
!Debug
makeDebug: args | cl meta n |
	args do: [:clname|
		" Map MetaFOO -> FOO class "
		((clname from: 1 to: 4) = 'Meta') ifTrue: [
			n <- clname from: 5.
			meta <- true
		] ifFalse: [
			n <- clname.
			meta <- false
		].

		" Look up class "
		cl <- Smalltalk at: n asSymbol ifAbsent: [ nil ].
		cl isNil
		 ifTrue: [ ('Unknown class: ' + clname) printNl ]
		 ifFalse: [
			" Map to metaclass if needed "
			meta ifTrue: [ cl <- cl class ].

			" Convert methods "
			cl methods keysDo: [:k| cl setDebug: k ]
		 ]
	]
!

!Debug
run | prev did cmd done line |
	" Main command loop for the debugger session "

	prev <- ''.
	done <- false.
	[true] whileTrue: [
		" Show where we are "
		self showLine.

		" Get command "
		'Debug> ' print.
		line <- String input.

		" Re-insert previous command if empty line "
		(line isEmpty) ifTrue: [ line <- prev ].
		prev <- line.

		" Parse into words "
		line <- line break: ' '.

		" Command is first, arguments follow "
		cmd <- line at: 1.
		line <- line from: 2 to: line size.

		" Set flag to indicate command hasn't matched yet "
		did <- false.

		" Step a single VM instruction "
		(cmd = 'stepi') ifTrue: [
			done
			 ifTrue: [ 'Not runnable' printNl ]
			 ifFalse: [
				prevList <- nil.
				(self runIt: 1) ifFalse: [
					done <- true
				]
			].
			did <- true
		].

		" Step a source line "
		((cmd = 'step') or: [cmd = 's']) ifTrue: [
			done
			 ifTrue: [ 'Not runnable' printNl ]
			 ifFalse: [
				 prevList <- nil.
				 done <- self doStep: true
			 ].
			did <- true
		].

		" Step a source line, stepping over message sends "
		((cmd = 'next') or: [cmd = 'n']) ifTrue: [
			done
			 ifTrue: [ 'Not runnable' printNl ]
			 ifFalse: [
				 prevList <- nil.
				 done <- self doStep: false
			 ].
			did <- true.
		].

		" Examine variables "
		((cmd = 'p') or: [cmd = 'print']) ifTrue: [
			self examine: line.
			did <- true
		].

		" Describe variable "
		(cmd = 'whatis') ifTrue: [
			self whatis: line.
			did <- true
		].

		" Set a breakpoint "
		((cmd = 'b') or: [cmd = 'break']) ifTrue: [
			self setBreak: line.
			did <- true
		].

		" Clear breakpoint(s) "
		((cmd = 'd') or: [cmd = 'delete']) ifTrue: [
			self clearBreak: line.
			did <- true
		].

		" List breakpoints "
		((cmd = 'lb') or: [cmd = 'listbreak']) ifTrue: [
			self listBreak.
			did <- true
		].

		" Just let it run "
		((cmd = 'cont') or: [cmd = 'c']) ifTrue: [
			" Clear previous listing position "
			prevList <- nil.

			" Step forward once, even over a breakpoint "
			done <- (self runIt: 1) not.

			" Now run until completion or next break "
			done ifFalse: [
				" Activate, run, and deactivate "
				self breakActive: true.
				done <- (self runIt: -1) not.
				self breakActive: false.

				" Display a message if hit a breakpoint "
				done ifFalse: [ self onBreak ].
			].

			did <- true
		].

		" Source listing "
		((cmd = 'l') or: [cmd = 'list']) ifTrue: [
			self list: line.
			did <- true
		].

		" Abandon the method "
		(cmd = 'quit') ifTrue: [
			^nil
		].

		" Stack backtrace "
		((cmd = 'where') or: [cmd = 'bt']) ifTrue: [
			proc context isNil
			ifTrue: [
				'Process has terminated' printNl
			] ifFalse: [
				proc context backtrace
			].
			did <- true
		].

		" Go up or down the stack chain "
		((cmd = 'up') or: [cmd = 'down']) ifTrue: [
			self upDown: (cmd = 'up') count: line.
			did <- true
		].

		" Make all procedures of the named class debuggable "
		(cmd = 'debug') ifTrue: [
			self makeDebug: line.
			did <- true
		].

		" Hook to data structure browser "
		((cmd = 'br') or: [cmd = 'browse']) ifTrue: [
			self browse: line.
			did <- true
		].

		" Error "
		did ifFalse: [ 'Unknown command.' printNl ].
	]
!
!String
asLines | c ret slot idx base top nl |
	" Convert a string with embedded newlines into an Array
	  with one slot per line.  The newlines are not a part of
	  these lines. "

	" Size the array of lines which will result "
	nl <- Char newline.
	ret <- Array new: (self occurencesOf: nl) + 1.

	" Walk the elements, assembling lines "
	slot <- base <- idx <- 1.
	top <- self size.
	[idx < top] whileTrue: [
		c <- self at: idx.
		(c = nl) ifTrue: [
			ret at: slot put: (self from: base to: idx - 1).
			slot <- slot + 1.
			base <- idx + 1
		].
		idx <- idx + 1
	].
	(idx > top) ifTrue: [ idx <- top ].
	(idx > base) ifTrue: [
		ret at: slot put: (self from: base to: idx)
	].
	^ ret
!
!DebugMethod
debug: l
	" Hook to initialize our DebugMethod instance "
	lines <- l.
	textlines <- text asLines.
	bpoints <- Dictionary new.
	active <- false
!
!DebugMethod
debug
	^ lines
!
!DebugMethod
textlines
	^ textlines
!
!DebugMethod
args: argNames inst: instNames temp: tempNames
	" Record debug data "
	vars <- Array new: 3.
	vars at: 1 put: argNames.
	vars at: 2 put: instNames.
	vars at: 3 put: tempNames
!
!DebugMethod
srcLine: bp
	" Map the VM instruction byte pointer onto a source line # "
	lines binaryDo: [:src :range |
		(range includes: bp) ifTrue: [
			^ src
		]
	].
	'No match for ' print. bp printString printNl.
	^nil
!
!DebugMethod
whatis: var in: ctx | idx obj |
	" Describe a variable in this Method "

	" Name of an argument "
	idx <- (vars at: 1) indexOfVal: var.
	idx notNil ifTrue: [
		var print. ' is an argument' print.
		obj <- ctx arguments at: idx
	].

	" Instance variable "
	obj isNil ifTrue: [
		idx <- (vars at: 2) indexOfVal: var.
		idx notNil ifTrue: [
			var print. ' is an instance variable' print.
			obj <- ctx arguments at: 1.
			obj <- (Object class in: obj at: idx)
		]
	].

	" Temporary "
	obj isNil ifTrue: [
		idx <- (vars at: 3) indexOfVal: var.
		idx notNil ifTrue: [
			var print. ' is a temporary variable' print.
			obj <- ctx temporaries at: idx
		]
	].

	" If we found it, display the generic information "
	obj notNil ifTrue: [
		' (index ' print. idx print. ')' printNl.
		' Value: ' print. obj printNl.
		' Class: ' print. obj class printString print.
		' basicSize: ' print. obj basicSize printNl.
	] ifFalse: [
		" Couldn't find it... "
		'Unknown variable' print. var printNl
	]
!
!DebugMethod
getVar: var in: ctx | idx |
	" Get a variable in this Method, return its value "

	" Name of an argument "
	idx <- (vars at: 1) indexOfVal: var.
	idx notNil ifTrue: [ ^ (ctx arguments at: idx) ].

	" Instance variable "
	idx <- (vars at: 2) indexOfVal: var.
	idx notNil ifTrue: [
		^ (Object in: (ctx arguments at: 1) at: idx)
	].

	" Temporary "
	idx <- (vars at: 3) indexOfVal: var.
	idx notNil ifTrue: [ ^ (ctx temporaries at: idx) ].

	" Couldn't find it... "
	^ nil
!
!DebugMethod
print: var in: ctx | obj |
	" Print a variable in this Method "
	obj <- self getVar: var in: ctx.
	obj isNil ifTrue: [ ('Unknown variable: ' + var) printNl ]
	 ifFalse: [ obj printNl ]
!
!DebugMethod
setBreak: bp | old |
	" Set a breakpoint in this Method "

	" If already set, ignore "
	(bpoints includes: bp) ifTrue: [ ^ self ].

	" Record current opcode at code location "
	old <- byteCodes at: (bp + 1).

	" Update the code location if it's already active "
	active ifTrue: [
		self patchBreak: bp active: true
	].

	" Record this breakpoint "
	bpoints at: bp put: old
!
!DebugMethod
clearBreak: bp
	" Remove a breakpoint in this Method "

	" If not set, ignore "
	(bpoints includes: bp) ifFalse: [ ^ self ].

	" Restore code contents "
	self patchBreak: bp active: false.

	" Remove record of this breakpoint "
	bpoints removeKey: bp
!
!DebugMethod
patchBreak: bp active: flag
	" Set or clear the breakpoint instruction in the code"

	flag ifTrue: [
		" Patch in a DoSpecial operation 12 (breakpoint) "
		byteCodes at: (bp + 1) put: ((15*16) + 12)
	] ifFalse: [
		" Restore old code at this location "
		byteCodes at: (bp + 1) put: (bpoints at: bp)
	]
!
!DebugMethod
breakActive: flag
	" Activate or deactivate breakpoints for this Method "

	" Skip all this if we aren't changing settings "
	(active = flag) ifTrue: [ ^ self ].

	" For each location with a breakpoint, update it "
	bpoints keysDo: [:bp|
		self patchBreak: bp active: flag
	].
	active <- flag
!
!DebugMethod
codeLoc: line | set ret |
	" Map source line # to a code location "

	" Get code generated for that line #, or return nil "
	set <- lines at: line ifAbsent: [ ^ nil ].

	" Return lowest code location "
	ret <- set anyOne.
	set do: [:elem| ret <- ret min: elem].
	^ ret
!
!Debug
browse: args | meth br ctx |
	" Get the DebugMethod, which has symbolic information for variables "
	ctx <- self curContext.
	meth <- ctx method.
	(meth isKindOf: DebugMethod) ifFalse: [
		'No debug information for ' print.
		meth name printNl.
		^ nil
	].

	" See if Browser is installed "
	br <- Smalltalk at: #Browser ifAbsent: [
		'Browser not installed.' printNl.
		^ nil
	].

	^ br on: (meth getVar: (args at: 1) in: ctx)
!

```

```st lst4/src/ImageBuilder/smalltalk/dict.st
+Set subclass: #Dict
+Dict subclass: #RotorDict variables: #(rotor)
!Dict
at: key ifAbsent: aBlock | pos assoc |
	pos <- self location: key.
	((pos isNil) or: [(assoc <- members at: pos) isNil]) ifTrue: [
		^ aBlock value
	].
	^ assoc value
!
!Dict
at: key put: value | assoc pos |
	" Create the Association "
	assoc <- Association key: key value: value.

	" Insert it using Set's method "
	self add: assoc.

	^ value
!
!Dict
binaryDo: aBlock
	members do: [:assoc|
		assoc notNil ifTrue: [ aBlock value: (assoc key)
			value: (assoc value) ]
	]
!
!Dict
keysDo: aBlock
	members do: [:assoc|
		assoc notNil ifTrue: [ aBlock value: (assoc key) ]
	]
!
!Dict
associationsDo: aBlock
	members do: [:assoc|
		assoc notNil ifTrue: [ aBlock value: assoc ]
	]
!
!Dict
basicDo: aBlock
	^ self associationsDo: aBlock
!
!Dict
keysAsArray | i ret |
	ret <- Array new: self size.
	i <- 1.
	members do: [:assoc| ret at: i put: assoc key. i <- i + 1 ].
	^ ret
!
!Dict
isEmpty
	members do: [:assoc| assoc notNil ifTrue: [ ^ false ]].
	^ true
!
!Dict
do: aBlock
	members do: [:assoc| assoc notNil ifTrue:
		[ aBlock value: (assoc value) ] ]
!
!Dict
removeKey: key ifAbsent: aBlock | assoc |
	assoc <- Association key: key value: nil.
	super remove: assoc ifAbsent: [ ^ aBlock value ].
	^ key
!
!Dict
removeKey: key
	^ self removeKey: key ifAbsent: [ self noElement ]
!
!Dict
remove: value ifAbsent: ignore
	self error: 'Can only remove by key'
!
!RotorDict
rotorDo: aBlock | start lim assoc |
	lim <- members size.
	rotor isNil ifTrue: [ rotor <- 1 ].
	start <- rotor.
	[true] whileTrue: [
		assoc <- members at: rotor.
		rotor <- rotor + 1.
		(rotor > lim) ifTrue: [ rotor <- 1 ].
		assoc notNil ifTrue: [ aBlock value: assoc ].
		(rotor = start) ifTrue: [ ^ self ]
	]
!
!RotorDict
do: aBlock
	^self rotorDo: [:assoc| aBlock value: assoc value]
!
!RotorDict
keysDo: aBlock
	^self rotorDo: [:assoc| aBlock value: assoc key]
!
!RotorDict
binaryDo: aBlock
	^self rotorDo: [:assoc| aBlock value: assoc key value: assoc value]
!

```

```st lst4/src/ImageBuilder/smalltalk/disasm.st
!Method
disassemble: indent at: initPC for: initCount | pc low high count needCR |
	pc <- initPC.
	count <- initCount.
	[ count > 0 ] whileTrue: [
		" Default, terminate line with CR "
		needCR <- true.

		" Show PC and indent listing of line "
		((pc rem: 20) = 0) ifTrue: [
			low <- String input.
			((low size > 0) and: [ (low at: 1) = $q ]) ifTrue: [
				self error: 'Disassembly halted'
			]
		].
		((pc - 1) printWidth: 4) print.
		1 to: indent do: [:x| ' ' print].

		" Fetch basic opcode fields "
		high <- byteCodes at: pc.
		pc <- pc + 1.
		count <- count - 1.
		low <- high rem: 16.
		high <- high quo: 16.
		(high = 0) ifTrue: [
			high <- low.
			low <- byteCodes at: pc.
			pc <- pc + 1.
			count <- count - 1
		].

		(high = 1) ifTrue: [
			'PushInstance ' print.
			low print
		].

		(high = 2) ifTrue: [
			'PushArgument ' print.
			low print
		].

		(high = 3) ifTrue: [
			'PushTemporary ' print.
			low print
		].

		(high = 4) ifTrue: [
			'PushLiteral ' print.
			low print
		].

		(high = 5) ifTrue: [
			'PushConstant ' print.
			((low >= 0) and: [low < 10]) ifTrue: [
				low print
			].
			(low = 10) ifTrue: [
				'nil' print
			].
			(low = 11) ifTrue: [
				'true' print
			].
			(low = 12) ifTrue: [
				'false' print
			]
		].

		(high = 6) ifTrue: [
			'AssignInstance ' print.
			low print
		].

		(high = 7) ifTrue: [
			'AssignTemporary ' print.
			low print
		].

		(high = 8) ifTrue: [
			'MarkArguments ' print.
			low print
		].

		(high = 9) ifTrue: [
			'SendMessage ' print.
			(literals at: (low+1)) print
		].

		(high = 10) ifTrue: [
			'SendUnary ' print.
			(low = 0) ifTrue: [ 'isNil' print ].
			(low = 1) ifTrue: [ 'notNil' print ]
		].

		(high = 11) ifTrue: [
			'SendBinary ' print.
			(low = 0) ifTrue: [ '<' print ].
			(low = 1) ifTrue: [ '<=' print ].
			(low = 2) ifTrue: [ '+' print ]
		].

		(high = 12) ifTrue: [
			'PushBlock' print. Char newline print.
			low <- byteCodes at: pc.
			pc <- pc + 1.
			low <- low + ((byteCodes at: pc) * 256).
			pc <- pc + 1.
			high <- low - (pc-1).
			self disassemble: indent+4 at: pc for: high.
			count <- count - (high + 2).
			pc <- pc + high.
			needCR <- false
		].

		(high = 13) ifTrue: [
			'DoPrimitive ' print.
			high <- byteCodes at: pc.
			pc <- pc + 1.
			count <- count - 1.

			" VM generic "
			(high = 1) ifTrue: [ '=' print ].
			(high = 2) ifTrue: [ 'class' print ].
			(high = 3) ifTrue: [ 'putchar' print ].
			(high = 4) ifTrue: [ 'size' print ].
			(high = 5) ifTrue: [ 'at:put:' print ].
			(high = 6) ifTrue: [ 'newProcess' print ].
			(high = 7) ifTrue: [ 'new' print ].
			(high = 8) ifTrue: [ 'block invoke' print ].
			(high = 9) ifTrue: [ 'getchar' print ].
			(high = 10) ifTrue: [ 'SmallInt +' print ].
			(high = 11) ifTrue: [ 'SmallInt /' print ].
			(high = 12) ifTrue: [ 'SmallInt %' print ].
			(high = 13) ifTrue: [ 'SmallInt <' print ].
			(high = 14) ifTrue: [ 'SmallInt =' print ].
			(high = 15) ifTrue: [ 'SmallInt *' print ].
			(high = 16) ifTrue: [ 'SmallInt -' print ].
			(high = 18) ifTrue: [ 'debug' print ].
			(high = 19) ifTrue: [ 'error' print ].
			(high = 20) ifTrue: [ 'ByteArray new' print ].
			(high = 21) ifTrue: [ 'String at:' print ].
			(high = 22) ifTrue: [ 'String at:put:' print ].
			(high = 23) ifTrue: [ 'String clone' print ].
			(high = 24) ifTrue: [ 'Array at:' print ].
			(high = 25) ifTrue: [ 'Integer /' print ].
			(high = 26) ifTrue: [ 'Integer %' print ].
			(high = 27) ifTrue: [ 'Integer +' print ].
			(high = 28) ifTrue: [ 'Integer *' print ].
			(high = 29) ifTrue: [ 'Integer -' print ].
			(high = 30) ifTrue: [ 'Integer <' print ].
			(high = 31) ifTrue: [ 'Integer =' print ].
			(high = 32) ifTrue: [ 'SmallInt asInteger' print ].
			(high = 33) ifTrue: [ 'Integer asSmallInt' print ].

			" UNIX port specific "
			(high = 100) ifTrue: [ 'UNIX fopen' print ].
			(high = 101) ifTrue: [ 'UNIX fgetc' print ].
			(high = 102) ifTrue: [ 'UNIX fputc' print ].
			(high = 103) ifTrue: [ 'UNIX fclose' print ].
			(high = 104) ifTrue: [ 'UNIX fileout' print ].
			(high = 105) ifTrue: [ 'UNIX edit' print ].
		].

		(high = 15) ifTrue: [
			'DoSpecial ' print.
			(low = 1) ifTrue: [ 'selfReturn' print ].
			(low = 2) ifTrue: [ 'stackReturn' print ].
			(low = 3) ifTrue: [ 'blockReturn' print ].
			(low = 4) ifTrue: [ 'duplicate' print ].
			(low = 5) ifTrue: [ 'popTop' print ].
			(low = 6) ifTrue: [
				'branch ' print.
				high <- byteCodes at: pc.
				pc <- pc + 1.
				high <- high + ((byteCodes at: pc) * 256).
				pc <- pc + 1.
				count <- count - 2.
				high print
			].
			(low = 7) ifTrue: [
				'branchIfTrue ' print.
				high <- byteCodes at: pc.
				pc <- pc + 1.
				high <- high + ((byteCodes at: pc) * 256).
				pc <- pc + 1.
				count <- count - 2.
				high print
			].
			(low = 8) ifTrue: [
				'branchIfFalse ' print.
				high <- byteCodes at: pc.
				pc <- pc + 1.
				high <- high + ((byteCodes at: pc) * 256).
				pc <- pc + 1.
				count <- count - 2.
				high print
			].
			(low = 11) ifTrue: [
				'sendToSuper ' print.
				low <- byteCodes at: pc.
				pc <- pc + 1.
				count <- count - 1.
				(literals at: low+1) print
			].
			(low = 12) ifTrue: [ 'breakpoint' print ].
		].
		needCR ifTrue: [
			Char newline print
		]
	]
!
!Method
disassemble
	self disassemble: 1 at: 1 for: (byteCodes size)
!

```

```st lst4/src/ImageBuilder/smalltalk/primes.st
!Number
isPrime
	((self rem: 2) = 0) ifTrue: [ ^ false ].
	(3 to: (self quo: 2) by: 2) do: [:cand|
		((self rem: cand) = 0) ifTrue: [ ^ false ]
	].
	^ true
!
=Number
primes: n | cand count |
	2 print.
	cand <- 3.
	count <- 0.
	[count < n] whileTrue: [
		[cand isPrime not] whileTrue: [
			cand <- cand + 2
		].
		' ' print. cand print.
		count <- count + 1.
		cand <- cand + 2
	].
	Char newline print
!

```

```lst4/src/LICENSE
Little Smalltalk, Version 4

Copyright (C) 1987-2005 by Timothy A. Budd
Copyright (C) 2007 by Charles R. Childers

============================================================================
This license applies to the virtual machine and to the initial image of 
the Little Smalltalk system and to all files in the Little Smalltalk 
packages.
============================================================================
Permission is hereby granted, free of charge, to any person obtaining a copy 
of this software and associated documentation files (the "Software"), to deal 
in the Software without restriction, including without limitation the rights 
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in 
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
DEALINGS IN THE SOFTWARE.

```

```lst4/src/Makefile
#
#	Makefile for Little Smalltalk system
#	written by Tim Budd, Oregon State University, budd@cs.orst.edu
#

CC=gcc
CFLAGS=-O2 -g -Wall
LIBS=

default:
	make st
	make image

st:
	$(CC) $(CFLAGS) source/*.c -o ./bin/st

image:
	cd ImageBuilder && make
	cp ImageBuilder/image bin/systemImage

clean:
	rm -f `find . | grep \~`
	rm -f bin/*
	cd ImageBuilder && make clean

```

```lst4/src/README
Little Smalltalk, Version 4
Timothy A. Budd, budd@cs.orst.edu
Oregon State University


I first started developing Little Smalltalk back in 1984, and to tell the
truth hadn't touched it for several years now.  However, when I attended
OOPSLA in Portland a few months ago, several things struck me:

 * Addison-Wesley was /still/ selling my book ``A Little Smalltalk''
   (despite it being many years old now and very very dated), and people were
   still buying it (don't ask me why).
 * There was a heck of a lot of interest in Smalltalk on the vendor floor.
 * People kept asking me about Little Smalltalk.

These things combined to make me think perhaps I should revisit Little 
Smalltalk.  In my spare time since then I've been updating the system, and 
this is the result.

What Little Smalltalk is, and more importantly, what it isn't

First and foremost, let me say at the beginning in as LOUD a voice as I can 
that Little Smalltalk is NOT a smalltalk-80 system. If you want a real
Smalltalk system go buy one from ParcPlace, Digitalk, IBM, or whomever.

Little Smalltalk is a sort of fun experiment in building a minimal 
smalltalk system.  The source code for the interpreter is less than 1800
lines of code.  The image consists of less than 4000 objects.  It runs
in almost no memory.  In short, it's small, it's reasonably fast, it's easy 
to understand, and easy to modify.

As you might expect, in order to get Smalltalk to run in something this
small, I've had to make lots of changes.  Here are the most notable:

 * No graphics
 * The user interface is a more conventional real-eval-print loop 
   (like most Lisp systems), not a browser. (But it is all written in
   Smalltalk itself, so you can modify it if you wish).
 * Primitives and cascades are handled as they were in earlier Little
   Smalltalk systems, which is not how they are handed in Smalltalk-80.
 * The standard class hierarchy differs considerably from Smalltalk-80.
   * My basic collections are List, Tree, Dictionary, Array, and String
   * The only numbers in the standard image are large and small integers
     (adding fractions and floats might be an interesting exercise, however).

```

```h lst4/src/source/globs.h
/*
 * globs.h
 *	Global defs for VM modules
 */
#ifndef GLOBS_H
#define GLOBS_H
#include <sys/types.h>
#include <stdio.h>

extern int fileIn(FILE * fp), fileOut(FILE * fp);
extern void sysError(char *, unsigned int), flushCache(void);
extern struct object *primitive(int, struct object *, int *);

#endif /* GLOBS_H */

```

```c lst4/src/source/interp.c
/* 
	Little Smalltalk version 4 

	Written by Tim Budd, Oregon State University, July 1994

    	budd@cs.orst.edu

    bytecode interpreter module

    given a process object, execute bytecodes in a tight loop.

    performs subroutine calls for
	a) garbage collection
        b) finding a non-cached method
        c) executing a primitive
	d) creating an integer

    otherwise simply loops until time slice has ended
*/

#include "memory.h"
#include "interp.h"
#include "globs.h"
#include <stdio.h>
#include <string.h>	/* For bzero() */

extern int debugging;
extern int cacheHit;
extern int cacheMiss;

/*
	The following are roots for the file out 
*/

struct object *nilObject, *trueObject, *falseObject,
	*SmallIntClass, *ArrayClass, *BlockClass, *ContextClass,
	*globalsObject, *initialMethod, *binaryMessages[3],
	*IntegerClass, *badMethodSym;

/*
 * Debugging
 */
#if defined(DEBUG) && defined(TRACE)
static void
indent(struct object *ctx)
{
	static int oldlev = 0;
	int lev = 0, x;

	while (ctx && (ctx != nilObject)) {
		lev += 1;
		putchar(' ');
		ctx = ctx->data[previousContextInContext];
	}

	/*
	 * This lets you use your editor's brace matching to
	 * match up opening and closing indentation levels.
	 */
	if (lev < oldlev) {
		for (x = lev; x < oldlev; ++x) {
			putchar('}');
		}
	} else if (lev > oldlev) {
		for (x = oldlev; x < lev; ++x) {
			putchar('{');
		}
	}

	oldlev = lev;
}
#define PC (bytePointer-1)
#define DBG0(msg) if (debugging) {indent(context); printf("%d: %s\n", PC, msg);}
#define DBG1(msg, arg) if (debugging) {indent(context); \
	printf("%d: %s %d\n", PC, msg, arg);}
#define DBGS(msg, cl, sel) \
	if (debugging) { \
		indent(context); \
		printf("%d: %s %s %s\n", PC, msg, cl, sel); }
#else
#define DBG0(msg)
#define DBG1(msg, arg)
#define DBGS(msg, cl, sel)
#endif

/*
	method lookup routine, used when cache miss occurs
*/

static int symbolcomp(struct object * left, struct object * right)
{
	int leftsize = SIZE(left);
	int rightsize = SIZE(right);
	int minsize = leftsize;
	register int i;

	if (rightsize < minsize) minsize = rightsize;
	for (i = 0; i < minsize; i++) {
		if (bytePtr(left)[i] != bytePtr(right)[i]) {
			return bytePtr(left)[i]-bytePtr(right)[i];
			}
		}
	return leftsize - rightsize;
}

static struct object *
lookupMethod(struct object *selector, struct object *class)
{
	struct object *dict, *keys, *vals, *val;
	uint low, high, mid;

	/*
	 * Scan upward through the class hierarchy
	 */
	for ( ; class != nilObject; class = class->data[parentClassInClass]) {
		/*
		 * Consider the Dictionary of methods for this Class
		 */
		dict = class->data[methodsInClass];
		keys = dict->data[0];
		low = 0; high = SIZE(keys);

		/*
		 * Do a binary search through its keys, which are
		 * Symbol's.
		 */
		while (low < high) {
			mid = (low + high) / 2;
			val = keys->data[mid];

			/* 
			 * If we find the selector, return the
			 * method object.
			 */
			if (val == selector) {
				vals = dict->data[1];
				return(vals->data[mid]);
			}

			/*
			 * Otherwise continue the binary search
			 */
			if (symbolcomp(selector, val) < 0) {
				high = mid;
			} else {
				low = mid+1;
			}
		}
	}

	/*
	 * Sorry, couldn't find a method
	 */
	return(NULL);
}

/*
	method cache for speeding method lookup
*/

# define cacheSize 703

static struct {
	struct object * name;
	struct object * class;
	struct object * method;
	} cache[cacheSize];

/* flush dynamic methods when GC occurs */
void
flushCache(void)
{
	int i;

	for (i = 0; i < cacheSize; i++) {
		cache[i].name = 0;	/* force refill */
	}
}

/*
 * newLInteger()
 *	Create new Integer (64-bit)
 */
static struct object *
newLInteger(long long val)
{
	struct object *res;

	res = gcialloc(sizeof(long long));
	res->class = IntegerClass;
	*(long long *)bytePtr(res) = val;
	return(res);
}

/*
 * do_Integer()
 *	Implement the appropriate 64-bit Integer operation
 *
 * Returns NULL on error, otherwise the resulting Integer or
 * Boolean (for comparisons) object.
 */
static struct object *
do_Integer(int op, struct object *low, struct object *high)
{
	long long l, h;

	l = *(long long *)bytePtr(low);
	h = *(long long *)bytePtr(high);
	switch (op) {
	case 25:	/* Integer division */
		if (h == 0LL) {
			return(NULL);
		}
		return(newLInteger(l/h));

	case 26:	/* Integer remainder */
		if (h == 0LL) {
			return(NULL);
		}
		return(newLInteger(l%h));

	case 27:	/* Integer addition */
		return(newLInteger(l+h));

	case 28:	/* Integer multiplication */
		return(newLInteger(l*h));

	case 29:	/* Integer subtraction */
		return(newLInteger(l-h));

	case 30:	/* Integer less than */
		return((l < h) ? trueObject : falseObject);

	case 31:	/* Integer equality */
		return((l == h) ? trueObject : falseObject);

	default:
		sysError("Invalid op table jump", op);
	}
	return(NULL);
}

/*
 * bulkReplace()
 *	Implement replaceFrom:to:with:startingAt: as a primitive
 *
 * Return 1 if we couldn't do it, 0 on success.  This routine has
 * distinct code paths for plain old byte type arrays, and for
 * arrays of object pointers; the latter must handle the special
 * case of static pointers.  It looks hairy (and it is), but it's
 * still much faster than executing the block move in Smalltalk
 * VM opcodes.
 */
static int
bulkReplace(struct object *dest, struct object *start,
	struct object *stop, struct object *src,
	struct object *repStart)
{
	int irepStart, istart, istop, count;

	/*
	 * We only handle simple 31-bit integer indices.  Map the
	 * values onto 0-based C array type values.
	 */
	if (!IS_SMALLINT(repStart) || !IS_SMALLINT(start) ||
			!IS_SMALLINT(stop)) {
		return(1);
	}
	irepStart = integerValue(repStart)-1;
	istart = integerValue(start)-1;
	istop = integerValue(stop)-1;
	count = (istop-istart) + 1;

	/*
	 * Defend against goofy negative indices.
	 */
	if ((irepStart < 0) || (istart < 0) || (istop < 0) ||
			(count < 1)) {
		return(1);
	}

	/*
	 * Range check
	 */
	if ((SIZE(dest) < istop) ||
			(SIZE(src) < (irepStart + count))) {
		return(1);
	}

	/*
	 * If both source and dest are binary, do a bcopy()
	 */
	if ((src->size & FLAG_BIN) && (dest->size & FLAG_BIN)) {
		/*
		 * Do it.
		 */
		bcopy(bytePtr(src) + irepStart, bytePtr(dest) + istart,
			count);
		return(0);
	}

	/*
	 * If not both regular storage, fail
	 */
	if ((src->size & FLAG_BIN) || (dest->size & FLAG_BIN)) {
		return(1);
	}

	/*
	 * If we're fiddling pointers between static and dynamic memory,
	 * let the VM-based implementation deal with it.
	 */
	if (isDynamicMemory(src) != isDynamicMemory(dest)) {
		return(1);
	}

	/*
	 * Copy object pointer fields
	 */
	bcopy(&src->data[irepStart], &dest->data[istart],
		BytesPerWord * count);
	return(0);
}

/* Code locations are extracted as VAL's */
#define VAL (bp[bytePointer] | (bp[bytePointer+1] << 8))
#define VALSIZE 2

int
execute(struct object *aProcess, int ticks)
{   
    int low, high, x, stackTop, bytePointer;
    struct object *context, *method, *arguments, *temporaries,
	    *instanceVariables, *literals, *stack,
	    *returnedValue = nilObject, *messageSelector,
	    *receiverClass, *op;
    unsigned char *bp;
    long long l;

    /* push process, so as to save it */
    rootStack[rootTop++] = aProcess;

    /* get current context information */
    context = aProcess->data[contextInProcess];

    method = context->data[methodInContext];

	/* load byte pointer */
    bp = (unsigned char *)bytePtr(method->data[byteCodesInMethod]);
    bytePointer = integerValue(context->data[bytePointerInContext]);

	/* load stack */
    stack = context->data[stackInContext];
    stackTop = integerValue(context->data[stackTopInContext]);

	/* everything else can wait, as maybe won't be needed at all */
    temporaries = instanceVariables = arguments = literals = 0;


    for (;;) {
	/*
	 * If we're running against a CPU tick count, stop execution
	 * when we expire the given number of ticks.
	 */
	if (ticks && (--ticks == 0)) {
		aProcess = rootStack[--rootTop];
		aProcess->data[contextInProcess] = context;
		aProcess->data[resultInProcess] = returnedValue;
		context->data[bytePointerInContext] = newInteger(bytePointer);
		context->data[stackTopInContext] = newInteger(stackTop);
		return(ReturnTimeExpired);
	}

	/* Otherwise decode the instruction */
        low = (high = bp[bytePointer++] ) & 0x0F;
        high >>= 4;
        if (high == Extended) {
            high = low;
            low = bp[bytePointer++] ;
        }

	/* And dispatch */
        switch (high) {

	case PushInstance:
	    DBG1("PushInstance", low);
	    if (! arguments) 
		    arguments = context->data[argumentsInContext];
	    if (! instanceVariables)
		    instanceVariables = 
			    arguments->data[receiverInArguments];
	    stack->data[stackTop++] = instanceVariables->data[low];
	    break;

	case PushArgument:
	    DBG1("PushArgument", low);
	    if (! arguments) 
		    arguments = context->data[argumentsInContext];
	    stack->data[stackTop++] = arguments->data[low];
	    break;

	case PushTemporary:
	    DBG1("PushTemporary", low);
	    if (! temporaries)
		    temporaries = context->data[temporariesInContext];
	    stack->data[stackTop++] = temporaries->data[low];
	    break;

	case PushLiteral:
	    DBG1("PushLiteral", low);
	    if (! literals) 
		    literals = method->data[literalsInMethod];
	    stack->data[stackTop++] = literals->data[low];
	    break;

	case PushConstant:
	    DBG1("PushConstant", low);
	    switch(low) {
		    case 0: case 1: case 2: case 3: case 4: 
		    case 5: case 6: case 7: case 8: case 9:
			    stack->data[stackTop++] = newInteger(low);
			    break;
		    case nilConst: 
			    stack->data[stackTop++] = nilObject; break;
		    case trueConst: 
			    stack->data[stackTop++] = trueObject; break;
		    case falseConst: 
			    stack->data[stackTop++] = falseObject; break;
		    default:
			    sysError("unknown push constant", low);
		    }
	    break;

	case PushBlock:
	    DBG0("PushBlock");
		    /* create a block object */
		    /* low is arg location */
		    /* next byte is goto value */
	    high = VAL;
	    bytePointer += VALSIZE;
	    rootStack[rootTop++] = context;
	    op = rootStack[rootTop++] = 
	     gcalloc(x = integerValue(method->data[stackSizeInMethod]));
	    op->class = ArrayClass;
	    bzero(bytePtr(op), x * BytesPerWord);
	    returnedValue = gcalloc(blockSize);
	    returnedValue->class = BlockClass;
	    returnedValue->data[bytePointerInContext] =
	    returnedValue->data[stackTopInBlock] = 
	    returnedValue->data[previousContextInBlock] = NULL;
	    returnedValue->data[bytePointerInBlock] =
		    newInteger(bytePointer);
	    returnedValue->data[argumentLocationInBlock] = newInteger(low);
	    returnedValue->data[stackInBlock] = rootStack[--rootTop];
	    context = rootStack[--rootTop];
	    if (CLASS(context) == BlockClass) {
		    returnedValue->data[creatingContextInBlock] =
			    context->data[creatingContextInBlock];
	    } else {
		    returnedValue->data[creatingContextInBlock] = context;
	    }
	    method = returnedValue->data[methodInBlock] = 
		    context->data[methodInBlock];
	    arguments = returnedValue->data[argumentsInBlock] =
		    context->data[argumentsInBlock];
	    temporaries = returnedValue->data[temporariesInBlock] =
		    context->data[temporariesInBlock];
	    stack = context->data[stackInContext];
	    bp = bytePtr(method->data[byteCodesInMethod]);
	    stack->data[stackTop++] = returnedValue;
		    /* zero these out just in case GC occurred */
	    literals = instanceVariables = 0;
	    bytePointer = high;
	    break;

	case AssignInstance:
	    DBG1("AssignInstance", low);
	    if (!arguments)  {
		    arguments = context->data[argumentsInContext];
	    }
	    if (!instanceVariables) {
		    instanceVariables = 
			    arguments->data[receiverInArguments];
			    /* don't pop stack, leave result there */
	    }
	    instanceVariables->data[low] = stack->data[stackTop-1];

	    /*
	     * If changing a static area, need to add to roots
	     */
	    if (!isDynamicMemory(instanceVariables) 
			    && isDynamicMemory(stack->data[stackTop-1])) {
		    addStaticRoot(&instanceVariables->data[low]);
	    }
	    break;

	case AssignTemporary:
	    DBG1("AssignTemporary", low);
	    if (! temporaries)
		    temporaries = context->data[temporariesInContext];
	    temporaries->data[low] = stack->data[stackTop-1];
	    break;

	case MarkArguments:
	    DBG1("MarkArguments", low);
	    rootStack[rootTop++] = context;
	    arguments = gcalloc(low);
	    arguments->class = ArrayClass;
	    if (context != rootStack[--rootTop]) { /* gc has occurred */
			    /* reload context */
		    instanceVariables = temporaries = literals = 0;
		    context = rootStack[rootTop];
		    method = context->data[methodInContext];
		    bp = bytePtr(method->data[byteCodesInMethod]);
		    stack = context->data[stackInContext];
	    }
	    /* now load new argument array */
	    while (low > 0) {
		    arguments->data[--low] = stack->data[--stackTop];
	    }
	    stack->data[stackTop++] = arguments;
	    arguments = 0;
	    break;

	case SendMessage: 
	    if (! literals) 
		    literals = method->data[literalsInMethod];
	    messageSelector = literals->data[low];
	    arguments = stack->data[--stackTop];

findMethodFromSymbol:
	    receiverClass = CLASS(arguments->data[receiverInArguments]);
	    DBGS("SendMessage",
		    bytePtr(receiverClass->data[nameInClass]),
		    bytePtr(messageSelector));
checkCache:
	    low = (((uint) messageSelector) +
		    ((uint) receiverClass)) % cacheSize;
	    if ((cache[low].name == messageSelector) &&
		(cache[low].class == receiverClass)) {
		    method = cache[low].method;
		    cacheHit++;
	    } else {
		    cacheMiss++;
		    method = lookupMethod(messageSelector, receiverClass);
		    if (!method) {
			    if (messageSelector == badMethodSym) {
				    sysError("doesNotUnderstand: missing", 0);
			    }
			    op = gcalloc(2);
			    op->class = ArrayClass;
			    op->data[receiverInArguments] =
				    arguments->data[receiverInArguments];
			    op->data[1] = messageSelector;
			    arguments = op;
			    messageSelector = badMethodSym;
			    goto findMethodFromSymbol;
		    }
		    cache[low].name = messageSelector;
		    cache[low].class = receiverClass;
		    cache[low].method = method;
	    }

	    /* see if we can optimize tail call */
	    if (bp[bytePointer] == (DoSpecial * 16 + StackReturn)) {
		    high = 1;
	    } else if (bp[bytePointer] ==
			    (DoSpecial * 16 + BlockReturn)) {
		    high = 2;
	    } else {
		    high = 0;
	    }

	    /* build temporaries for new context */
	    rootStack[rootTop++] = arguments;
	    rootStack[rootTop++] = method;
	    rootStack[rootTop++] = context;
	    low = integerValue(method->data[temporarySizeInMethod]);
	    op = rootStack[rootTop++] = 
	     gcalloc(x = integerValue(method->data[stackSizeInMethod]));
	    op->class = ArrayClass;
	    bzero(bytePtr(op), x * BytesPerWord);
	    if (low > 0) {
		    int i;

		    temporaries = gcalloc(low);
		    temporaries->class = ArrayClass;
		    for (i = 0; i < low; i++) {
			    temporaries->data[i] = nilObject;
		    }
		    rootStack[rootTop++] = temporaries; /* temporaries */
	    } else {
		    rootStack[rootTop++] = NULL;	/* why bother */
	    }
	    context = rootStack[rootTop-3];
	    context->data[stackTopInContext] = newInteger(stackTop);
	    context->data[bytePointerInContext] = newInteger(bytePointer);

	    /* now go off and build the new context */
	    context = gcalloc(contextSize);
	    context->class = ContextClass;
	    temporaries = context->data[temporariesInContext] 
		    = rootStack[--rootTop];
	    stack = context->data[stackInContext] = rootStack[--rootTop];
	    stack->class = ArrayClass;
	    context->data[stackTopInContext] = newInteger(0);
	    stackTop = 0;
	    context->data[previousContextInContext] = rootStack[--rootTop];
	    if (high == 1) {
		    context->data[previousContextInContext] =
		    context->data[previousContextInContext]->
			    data[previousContextInContext];
	    } else if (high == 2) {
		    context->data[previousContextInContext] =
		    context->data[previousContextInContext]->
			    data[creatingContextInBlock]->
			    data[previousContextInContext];
	    }
	    method = context->data[methodInContext] = rootStack[--rootTop];
	    arguments = context->data[argumentsInContext] 
		    = rootStack[--rootTop];
	    instanceVariables = literals = 0;
	    context->data[bytePointerInContext] = newInteger(0);
	    bytePointer = 0;
	    bp = (char *) method->data[byteCodesInMethod]->data;
		    /* now go execute new method */
	    break;

	case SendUnary:	/* optimize certain unary messages */
	    DBG1("SendUnary", low);
	    returnedValue = stack->data[--stackTop];
	    switch(low) {
		    case 0:	/* isNil */
			    if (returnedValue == nilObject)
				    returnedValue = trueObject;
			    else
				    returnedValue = falseObject;
			    break;
		    case 1: /* notNil */
			    if (returnedValue == nilObject)
				    returnedValue = falseObject;
			    else
				    returnedValue = trueObject;
			    break;
		    default:
			    sysError("unimplemented SendUnary", low);
		    }
	    stack->data[stackTop++] = returnedValue;
	    break;

	case SendBinary:	/* optimize certain binary messages */
	    DBG1("SendBinary", low);
	    if (IS_SMALLINT(stack->data[stackTop-1])
		     && IS_SMALLINT(stack->data[stackTop-2])) {
		    int i, j;
		    j = integerValue(stack->data[--stackTop]);
		    i = integerValue(stack->data[--stackTop]);
			    /* can only do operations that won't */
			    /* trigger garbage collection */
		    switch(low) {
		    case 0:	/* < */
			    if (i < j) {
				    returnedValue = trueObject;
			    } else {
				    returnedValue = falseObject;
			    }
			    break;
		    case 1:	/* <= */
			    if (i <= j) {
				    returnedValue = trueObject;
			    } else {
				    returnedValue = falseObject;
			    }
			    break;
		    case 2:	/* + */
			    /* no possibility of garbage col */
			    returnedValue = newInteger(i+j);
			    break;
		    }
		    stack->data[stackTop++] = returnedValue;
		    break;
	    }

	    /* not integers, do as message send */
	    rootStack[rootTop++] = context;
	    arguments = gcalloc(2);
	    arguments->class = ArrayClass;
	    if (context != rootStack[--rootTop]) { /* gc has occurred */
			    /* reload context */
		    instanceVariables = temporaries = literals = 0;
		    context = rootStack[rootTop];
		    method = context->data[methodInContext];
		    bp = bytePtr(method->data[byteCodesInMethod]);
		    stack = context->data[stackInContext];
	    }

	    /* now load new argument array */
	    arguments->data[1] = stack->data[--stackTop];
	    arguments->data[0] = stack->data[--stackTop];
		    /* now go send message */
	    messageSelector = binaryMessages[low];
	    goto findMethodFromSymbol;

/*
* Pull two integers of the required class as arguments from stack
*/
#define GET_HIGH_LOW() \
    op = stack->data[--stackTop]; \
    if (!IS_SMALLINT(op)) { \
	    stackTop -= 1; \
	    goto failPrimitive; \
    } \
    low = integerValue(op); \
    op = stack->data[--stackTop]; \
    if (!IS_SMALLINT(op)) { \
	    goto failPrimitive; \
    } \
    high = integerValue(op);

	case DoPrimitive:
		    /* low is argument count */
		    /* next byte is primitive number */
	    high = bp[bytePointer++];
	    DBG1("DoPrimitive", high);
	    rootStack[rootTop++] = context;
	    switch (high) {
	    case 1:		/* object identity */
		    returnedValue = stack->data[--stackTop];
		    if (returnedValue == stack->data[--stackTop]) {
			    returnedValue = trueObject;
		    } else {
			    returnedValue = falseObject;
		    }
		    break;

	    case 2:		/* object class */
		    returnedValue = stack->data[--stackTop];
		    returnedValue = CLASS(returnedValue);
		    break;

	    case 3:	/* print a single character */
		    low = integerValue(stack->data[--stackTop]);
		    putchar(low); /* fflush(stdout); */
		    returnedValue = nilObject;
		    break;

	    case 4:	/* object size */
		    returnedValue = stack->data[--stackTop];
		    if (IS_SMALLINT(returnedValue)) {
			    high  = 0;
		    } else {
			    high = SIZE(returnedValue);
		    }
		    returnedValue = newInteger(high);
		    break;

	    case 5:		/* Array at put */
		    op = stack->data[--stackTop];
		    if (!IS_SMALLINT(op)) {
			    stackTop -= 2;
			    goto failPrimitive;
		    }
		    low = integerValue(op)-1;
		    returnedValue = stack->data[--stackTop];
		    /* Bounds check */
		    if ((low < 0) ||
		     (low >= SIZE(returnedValue))) {
			    stackTop -= 1;
			    goto failPrimitive;
		    }
		    returnedValue->data[low] 
			    = stack->data[--stackTop];
		    /*
		     * If putting a non-static pointer
		     * into an array in static memory,
		     * register it for GC.
		     */
		    if (!isDynamicMemory(returnedValue) 
				    && isDynamicMemory(
				     stack->data[stackTop])) {
			    addStaticRoot(
			     &returnedValue->data[low]);
		    }
		    break;

	    case 6:		/* new process execute */
		    low = integerValue(stack->data[--stackTop]);
		    op = stack->data[--stackTop];
		    low = execute(op, low);

		    /* return value as a SmallInt */
		    returnedValue = newInteger(low);
		    break;

	    case 7: 	/* new object allocation */
		    low = integerValue(stack->data[--stackTop]);
		    rootStack[rootTop++] = stack->data[--stackTop];
		    returnedValue = gcalloc(low);
		    returnedValue->class = rootStack[--rootTop];
		    while (low > 0) {
			    returnedValue->data[--low] = nilObject;
		    }
		    break;

	    case 8:	/* block invocation */
		    /* low holds number of arguments */
		    returnedValue = stack->data[--stackTop];
			    /* put arguments in place */
		    high = integerValue(returnedValue->data[
				    argumentLocationInBlock]);
		    temporaries = returnedValue->data[temporariesInBlock];
		    low -= 2;
		    x = (temporaries ?
			    (SIZE(temporaries) - high) : 0);
		    if (low >= x) {
			    stackTop -= (low+1);
			    goto failPrimitive;
		    }
		    while (low >= 0) {
			    temporaries->data[high + low] =
				    stack->data[--stackTop];
			    low--;
		    }
		    returnedValue->data[previousContextInBlock] =
			    context->data[previousContextInContext];
		    context = returnedValue;
		    arguments = instanceVariables =
			    literals = 0;
		    stack = context->data[stackInContext];
		    stackTop = 0;
		    method = context->data[methodInBlock];
		    bp = bytePtr(method->data[byteCodesInMethod]);
		    bytePointer = integerValue(
			    context->data[bytePointerInBlock]);
		    --rootTop;
		    goto endPrimitive;

	    case 9:		/* read char from input */
		    low = getchar();
		    if (low == EOF) {
			    returnedValue = nilObject;
		    } else {
			    returnedValue = newInteger(low);
		    }
		    break;

	    case 10: 	/* small integer addition */
		    GET_HIGH_LOW();
		    x = high + low;
		    if (((high > 0) && (low > 0) && (x < high)) ||
		     ((high < 0) && (low < 0) && (x > high))) {
			    /* overflow... do it with 64 bits */
			    returnedValue = newLInteger(
				    (long long)high + (long long)low);
		    } else {
			    if (!FITS_SMALLINT(x)) {
				    returnedValue = newLInteger(x);
			    } else {
				    returnedValue = newInteger(x);
			    }
		    }
		    break;
		    
	    case 11: 	/* small integer division */
		    GET_HIGH_LOW();
		    if (low == 0) {
			    goto failPrimitive;
		    }
		    high /= low;
		    returnedValue = newInteger(high);
		    break;
		    
	    case 12:	/* small integer remainder */ 
		    GET_HIGH_LOW();
		    if (low == 0) {
			    goto failPrimitive;
		    }
		    high %= low;
		    returnedValue = newInteger(high);
		    break;
		    
	    case 13:	/* small integer less than */ 
		    GET_HIGH_LOW();
		    if (high < low) {
			    returnedValue = trueObject;
		    } else {
			    returnedValue = falseObject;
		    }
		    break;

	    case 14:	/* small integer equality */ 
		    GET_HIGH_LOW();
		    if (high == low) {
			    returnedValue = trueObject;
		    } else {
			    returnedValue = falseObject;
		    }
		    break;

	    case 15:	/* small integer multiplication */ 
		    GET_HIGH_LOW();
		    x = high*low;
		    if ((low == 0) || (x/low == high)) {
			    if (!FITS_SMALLINT(x)) {
				    returnedValue = newLInteger(x);
			    } else {
				    returnedValue = newInteger(x);
			    }
		    } else {
			    /* overflow... do it with 64 bits */
			    returnedValue = newLInteger(
				    (long long)high * (long long)low);
		    }
		    break;

	    case 16:	/* small integer subtraction */ 
		    GET_HIGH_LOW();
		    x = high - low;
		    if ((low > 0) && (high < 0) && (x > high)) {
			    returnedValue = newLInteger(
				    (long long)high - (long long)low);
		    } else {
			    if (!FITS_SMALLINT(x)) {
				    returnedValue = newLInteger(x);
			    } else {
				    returnedValue = newInteger(x);
			    }
		    }
		    break;

	    case 18: 	/* turn on debugging */
		    debugging = 1;
		    returnedValue = nilObject;
		    break;

	    case 19:	/* error trap -- halt execution */
		    --rootTop; /* pop context */
		    aProcess = rootStack[--rootTop];
		    aProcess->data[contextInProcess] = context;
		    return(ReturnError);

	    case 20:	/* byteArray allocation */
		    low = integerValue(stack->data[--stackTop]);
		    rootStack[rootTop++] = stack->data[--stackTop];
		    returnedValue = gcialloc(low);
		    returnedValue->class = rootStack[--rootTop];
		    bzero(bytePtr(returnedValue), low);
		    break;

	    case 21:	/* string at */
		    low = integerValue(stack->data[--stackTop])-1;
		    returnedValue = stack->data[--stackTop];
		    if ((low < 0) ||
		     (low >= SIZE(returnedValue))) {
			    goto failPrimitive;
		    }
		    low = bytePtr(returnedValue)[low];
		    returnedValue = newInteger(low);
		    break;

	    case 22:	/* string at put */
		    low = integerValue(stack->data[--stackTop])-1;
		    returnedValue = stack->data[--stackTop];
		    if ((low < 0) ||
		     (low >= SIZE(returnedValue))) {
			    stackTop -= 1;
			    goto failPrimitive;
		    }
		    bytePtr(returnedValue)[low] =
			    integerValue(stack->data[--stackTop]);
		    break;

	    case 23:	/* string clone */
		    rootStack[rootTop++] = stack->data[--stackTop];
		    rootStack[rootTop++] = returnedValue 
			    = stack->data[--stackTop];
		    low = SIZE(returnedValue);
		    returnedValue = gcialloc(low);
		    messageSelector = rootStack[--rootTop];
		    while (low-- > 0)
			    bytePtr(returnedValue)[low] =
				    bytePtr(messageSelector)[low];
		    returnedValue->class = rootStack[--rootTop];
		    break;

	    case 24:	/* array at */
		    low = integerValue(stack->data[--stackTop])-1;
		    returnedValue = stack->data[--stackTop];
		    if ((low < 0) ||
		     (low >= SIZE(returnedValue))) {
			    goto failPrimitive;
		    }
		    returnedValue = returnedValue->data[low];
		    break;
#undef GET_HIGH_LOW

	    case 25:	/* Integer division */
	    case 26:	/* Integer remainder */
	    case 27:	/* Integer addition */
	    case 28:	/* Integer multiplication */
	    case 29:	/* Integer subtraction */
	    case 30:	/* Integer less than */
	    case 31:	/* Integer equality */
		    op = stack->data[--stackTop];
		    if (CLASS(op) != IntegerClass) {
			    stackTop -= 1;
			    goto failPrimitive;
		    }
		    returnedValue = stack->data[--stackTop];
		    if (CLASS(returnedValue) != IntegerClass) {
			    goto failPrimitive;
		    }
		    returnedValue = do_Integer(high,
			    returnedValue, op);
		    if (returnedValue == NULL) {
			    goto failPrimitive;
		    }
		    break;

	    case 32:	/* Integer allocation */
		    op = stack->data[--stackTop];
		    if (!IS_SMALLINT(op)) {
			    goto failPrimitive;
		    }
		    returnedValue = newLInteger(integerValue(op));
		    break;

	    case 33:	/* Low order of Integer -> SmallInt */
		    op = stack->data[--stackTop];
		    l = *(long long *)bytePtr(op);
		    x = l;
		    if (!FITS_SMALLINT(x)) {
			    goto failPrimitive;
		    }
		    returnedValue = newInteger(x);
		    break;

	    case 34:	/* Flush method cache */
	    	    flushCache();
		    break;

	    case 35:	/* Bulk object exchange */
	    	    op = stack->data[--stackTop];
		    if (op->class != ArrayClass) {
		    	goto failPrimitive;
		    }
		    returnedValue = stack->data[--stackTop];
		    if (returnedValue->class != ArrayClass) {
		    	goto failPrimitive;
		    }
		    if (SIZE(op) != SIZE(returnedValue)) {
		    	goto failPrimitive;
		    }
		    exchangeObjects(op, returnedValue, SIZE(op));
		    break;

	    case 36:	/* bitOr: */
	    	op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			--stackTop;
			goto failPrimitive;
		}
		high = integerValue(op);
		op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			goto failPrimitive;
		}
		returnedValue = newInteger(integerValue(op) | high);
		break;

	    case 37:	/* bitAnd: */
	    	op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			--stackTop;
			goto failPrimitive;
		}
		high = integerValue(op);
		op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			goto failPrimitive;
		}
		returnedValue = newInteger(integerValue(op) & high);
		break;

	    case 38:	/* replaceFrom:... */
		returnedValue = stack->data[--stackTop];
	    	if (bulkReplace(returnedValue,
			stack->data[--stackTop],
			stack->data[--stackTop],
			stack->data[--stackTop],
			stack->data[--stackTop])) {
		    goto failPrimitive;
		}
		break;

	    case 39:	/* bitShift: */
	    	op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			--stackTop;
			goto failPrimitive;
		}
		low = integerValue(op);
		op = stack->data[--stackTop];
		if (!IS_SMALLINT(op)) {
			goto failPrimitive;
		}
		high = integerValue(op);
		if (low < 0) {
			/* Negative means shift right */
			low = high >> (-low);
		} else {
			/* Shift left--catch overflow */
			low = high << low;
			if (high > low) {
				goto failPrimitive;
			}
		}
		returnedValue = newInteger(low);
		break;

	    case 40:	/* Truncate Integer -> SmallInt */
	    	op = stack->data[--stackTop];
		l = *(long long *)bytePtr(op);
		x = l;
		returnedValue = newInteger(x);
		break;

	    default:
			    /* pop arguments, try primitive */
		    rootStack[rootTop++] = stack;
		    arguments = gcalloc(low);
		    arguments->class = ArrayClass;
		    stack = rootStack[--rootTop];
		    while (low > 0) {
			    arguments->data[--low] = 
				    stack->data[--stackTop];
		    }
		    {
			int failed;

			returnedValue =
				primitive(high, arguments, &failed);
		    	if (failed) {
				goto failPrimitive;
			}
		    }
		    arguments = 0;
		    break;
	    }

	    /*
	     * Restore our context pointer and
	     * force a stack return due to successful
	     * primitive.
	     */
	    context = rootStack[--rootTop];
	    goto doReturn;

failPrimitive:
	    /*
	     * Since we're continuing execution from a failed
	     * primitive, re-fetch context if a GC had occurred
	     * during the failed execution.  Supply a return value
	     * for the failed primitive.
	     */
	    returnedValue = nilObject;
	    if (context != rootStack[--rootTop]) {
		    context = rootStack[rootTop];
		    method = context->data[methodInContext];
		    stack = context->data[stackInContext];
		    bp = bytePtr(method->data[byteCodesInMethod]);
		    arguments = temporaries = literals = 
			    instanceVariables = 0;
	    }
	    stack->data[stackTop++] = nilObject;

endPrimitive:
	    /*
	     * Done with primitive, continue execution loop
	     */
	    break;

	case DoSpecial:
	    DBG1("DoSpecial", low);
	    switch(low) {
		case SelfReturn:
		    if (! arguments) 
			    arguments = context->data[argumentsInContext];
		    returnedValue = arguments->data[receiverInArguments];
		    goto doReturn;

		case StackReturn:
		    returnedValue = stack->data[--stackTop];

doReturn:
		    context = context->data[previousContextInContext];
doReturn2:
		    if ((context == 0) || (context == nilObject)) {
			    aProcess = rootStack[--rootTop];
			    aProcess->data[contextInProcess] = context;
			    aProcess->data[resultInProcess] = returnedValue;
			    return(ReturnReturned);
			    }
		    arguments = instanceVariables = literals 
			    = temporaries = 0;
		    stack = context->data[stackInContext];
		    stackTop = 
			    integerValue(context->data[stackTopInContext]);
		    stack->data[stackTop++] = returnedValue;
		    method = context->data[methodInContext];
		    bp = bytePtr(method->data[byteCodesInMethod]);
		    bytePointer = 
			    integerValue(context->data[bytePointerInContext]);
		    break;

		case BlockReturn:
		    returnedValue = stack->data[--stackTop];
		    context = context->data[creatingContextInBlock]
			    ->data[previousContextInContext];
		    goto doReturn2;

		case Duplicate:
		    returnedValue = stack->data[stackTop-1];
		    stack->data[stackTop++] = returnedValue;
		    break;

		case PopTop:
		    stackTop--;
		    break;

		case Branch:
		    low = VAL;
		    bytePointer = low;
		    break;

		case BranchIfTrue:
		    low = VAL;
		    returnedValue = stack->data[--stackTop];
		    if (returnedValue == trueObject) {
			    bytePointer = low;
		    } else {
			    bytePointer += VALSIZE;
		    }
		    break;

		case BranchIfFalse:
		    low = VAL;
		    returnedValue = stack->data[--stackTop];
		    if (returnedValue == falseObject) {
			    bytePointer = low;
		    } else {
			    bytePointer += VALSIZE;
		    }
		    break;

		case SendToSuper:
		    /* next byte has literal selector number */
		    low = bp[bytePointer++];
		    if (! literals) {
			    literals = method->data[literalsInMethod];
		    }
		    messageSelector = literals->data[low];
		    receiverClass = 
			    method->data[classInMethod]
				    ->data[parentClassInClass];
		    arguments = stack->data[--stackTop];
		    goto checkCache;

		case Breakpoint:
		    /* Back up on top of the breaking location */
		    bytePointer -= 1;

		    /* Return to our master process */
		    aProcess = rootStack[--rootTop];
		    aProcess->data[contextInProcess] = context;
		    aProcess->data[resultInProcess] = returnedValue;
		    context->data[bytePointerInContext] =
			newInteger(bytePointer);
		    context->data[stackTopInContext] = newInteger(stackTop);
		    return(ReturnBreak);

		default:
		    sysError("invalid doSpecial", low);
		    break;
	    }
	    break;

	default:
	    sysError("invalid bytecode", high);
	    break;
        }
    }
}

```

```h lst4/src/source/interp.h
/*
	Little Smalltalk, version 4
	Written by Tim Budd, Oregon State University, budd@cs.orst.edu
	All rights reserved, no guarantees given whatsoever.
	May be freely redistributed if not for profit.
*/

int execute(struct object *aProcess, int ticks);

/*
	symbolic definitions for the bytecodes
*/

# define Extended 0
# define PushInstance 1
# define PushArgument 2
# define PushTemporary 3
# define PushLiteral 4
# define PushConstant 5
# define AssignInstance 6
# define AssignTemporary 7
# define MarkArguments 8
# define SendMessage 9
# define SendUnary 10
# define SendBinary 11
# define PushBlock 12
# define DoPrimitive 13
# define DoSpecial 15

/* types of special instructions (opcode 15) */

# define SelfReturn 1
# define StackReturn 2
# define BlockReturn 3
# define Duplicate 4
# define PopTop 5
# define Branch 6
# define BranchIfTrue 7
# define BranchIfFalse 8
# define SendToSuper 11
# define Breakpoint 12

/* special constants */

/* constants 0 to 9 are the integers 0 to 9 */
# define nilConst 10
# define trueConst 11
# define falseConst 12

/* Return values from doExecute: primitive */
#define ReturnError 2		/* error: called */
#define ReturnBadMethod 3	/* Unknown method for object */
#define ReturnReturned 4	/* Top level method returned */
#define ReturnTimeExpired 5	/* Time quantum exhausted */
#define ReturnBreak 6		/* Breakpoint instruction */

/*
	The following are the objects with ``hard-wired'' 
	field offsets
*/
/*
	A Process has two fields
		* a current context
		* status of process (running, waiting, etc)
*/

# define contextInProcess 0
# define statusInProcess 1
# define resultInProcess 2

/*
	A Context has:
		* method (which has bytecode pointer)
		* bytecode offset (an integer)
		* arguments
		* temporaries and stack
		* stack pointer
*/

# define contextSize 7
# define methodInContext 0
# define argumentsInContext 1
# define temporariesInContext 2
# define stackInContext 3
# define bytePointerInContext 4
# define stackTopInContext 5
# define previousContextInContext 6

/*
	A Block is subclassed from Context
	shares fields with creator, plus a couple new ones
*/

# define blockSize 10
# define methodInBlock methodInContext
# define argumentsInBlock argumentsInContext
# define temporariesInBlock temporariesInContext
# define stackInBlock stackInContext
# define stackTopInBlock stackTopInContext
# define previousContextInBlock previousContextInContext
# define argumentLocationInBlock 7
# define creatingContextInBlock 8
/* the following are saved in different place so they don't get clobbered*/
# define bytePointerInBlock 9

/*
	A Method has:
		* name (a Symbol)
		* bytecodes
		* literals
		* stack size
		* temp size
*/

# define methodSize 7
# define nameInMethod 0
# define byteCodesInMethod 1
# define literalsInMethod 2
# define stackSizeInMethod 3
# define temporarySizeInMethod 4
# define classInMethod 5
# define textInMethod 6

/*
	A Class has:
		* pointer to parent class
		* pointer to tree of methods
*/

# define ClassSize 5
# define nameInClass 0
# define parentClassInClass 1
# define methodsInClass 2
# define instanceSizeInClass 3
# define variablesInClass 4

/*
	A node in a tree has
		* value field
		* left subtree
		* right subtree
*/

# define valueInNode 0
# define leftInNode 1
# define rightInNode 2

/*
	misc defines
*/

# define rootInTree 0
# define receiverInArguments 0
# define symbolsInSymbol 5

```

```c lst4/src/source/main.c
/*
	Little Smalltalk main program, unix version
	Written by Tim Budd, budd@cs.orst.edu
	All rights reserved, no guarantees given whatsoever.
	May be freely redistributed if not for profit.

	starting point, primitive handler for unix
	version of the little smalltalk system
*/
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include "globs.h"

/*
	the following defaults must be set

*/
#define DefaultImageFile "systemImage"
#define DefaultStaticSize 40000
#define DefaultDynamicSize 40000
#define DefaultTmpdir "/tmp"

/*
--------------------
*/

#include "memory.h"
#include "interp.h"
#include <stdio.h>

/* #define COUNTTEMPS */

unsigned int debugging = 0, cacheHit = 0, cacheMiss = 0, gccount = 0;
static char *tmpdir = DefaultTmpdir;

void
sysError(char * a, unsigned int b)
{
	fprintf(stderr,"unrecoverable system error: %s 0x%x\n", a, b);
	exit(1);
}

static void
backTrace(struct object * aContext)
{
	printf("back trace\n");
	while (aContext && (aContext != nilObject)) {
		struct object * arguments; int i;
		printf("message %s ", 
			bytePtr(aContext->data[methodInContext]
				->data[nameInMethod]));
		arguments = aContext->data[argumentsInContext];
		if (arguments && (arguments != nilObject)) {
			printf("(");
			for (i = 0; i < SIZE(arguments); i++)
				printf("%s%s", 
				((i == 0) ? "" : ", "),
				bytePtr(arguments->data[i]->class->
					data[nameInClass]));
			printf(")");
			}
		printf("\n");
		aContext = aContext->data[previousContextInContext];
		}
}

# ifdef COUNTTEMPS
FILE * tempFile;
# endif

int
main(int argc, char ** argv)
{
	struct object *aProcess, *aContext, *o;
	int size, i, staticSize, dynamicSize;
	FILE *fp;
	char imageFileName[120], *p;

	strcpy(imageFileName, DefaultImageFile);
	staticSize = DefaultStaticSize;
	dynamicSize = DefaultDynamicSize;

	/*
	 * See if our environment tells us what TMPDIR to use
	 */
	p = getenv("TMPDIR");
	if (p) {
		tmpdir = strdup(p);
	}

	/* first parse arguments */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-v") == 0) {
			printf("Little Smalltalk, version 4.01\n");
			}
		else if (strcmp(argv[i], "-s") == 0) {
			staticSize = atoi(argv[++i]);
		} else if (strcmp(argv[i], "-d") == 0) {
			dynamicSize = atoi(argv[++i]);
		} else if (strcmp(argv[i], "-g") == 0) {
			debugging = 1;
		} else {
			strcpy(imageFileName, argv[i]);
			}
		}

# ifdef COUNTTEMPS
	tempFile = fopen("/usr/tmp/counts", "w");
# endif

	gcinit(staticSize, dynamicSize);

	/* read in the method from the image file */
	fp = fopen(imageFileName, "r");
	if (! fp) {
		fprintf(stderr,"cannot open image file: %s\n", imageFileName);
		exit(1);
		}

	printf("%d objects in image\n", fileIn(fp));
	fclose(fp);

	/* build a context around it */

	aProcess = staticAllocate(3);
		/* context should be dynamic */
	aContext = gcalloc(contextSize);
	aContext->class = ContextClass;


	aProcess->data[contextInProcess] = aContext;
	size = integerValue(initialMethod->data[stackSizeInMethod]);
	aContext->data[stackInContext] = staticAllocate(size);
	aContext->data[argumentsInContext] = nilObject;

	aContext->data[temporariesInContext] = staticAllocate(19);
	aContext->data[bytePointerInContext] = newInteger(0);
	aContext->data[stackTopInContext] = newInteger(0);
	aContext->data[previousContextInContext] = nilObject;
	aContext->data[methodInContext] = initialMethod;

	/* now go do it */
	rootStack[rootTop++] = aProcess;

	switch(execute(aProcess, 0)) {
		case 2: printf("User defined return\n"); break;

		case 3: printf("can't find method in call\n"); 
			aProcess = rootStack[--rootTop];
			o = aProcess->data[resultInProcess];
			printf("Unknown method: %s\n", bytePtr(o));
			aContext = aProcess->data[contextInProcess];
			backTrace(aContext);
			break;

		case 4: printf("\nnormal return\n"); break;

		case 5: printf("time out\n"); break;

		default: printf("unknown return code\n"); break;
	}
	printf("cache hit %u miss %u", cacheHit, cacheMiss);
#define SCALE (1000)
	while ((cacheHit > INT_MAX/SCALE) || (cacheMiss > INT_MAX/SCALE)) {
		cacheHit /= 10;
		cacheMiss /= 10;
	}
	i = (SCALE * cacheHit) / (cacheHit + cacheMiss);
	printf(" ratio %u.%u%%\n", i / 10, i % 10);
	printf("%u garbage collections\n", gccount);
	return(0);
}

/*
	primitive handler
	(note that many primitives are handled in the interpreter)
*/

#define FILEMAX 20
static FILE *filePointers[FILEMAX];

static void
getUnixString(char * to, int size, struct object * from)
{
	int i;
	int fsize = SIZE(from);
	struct byteObject * bobj = (struct byteObject *) from;

	if (fsize > size) {
		sysError("error converting text into unix string", fsize);
	}
	for (i = 0; i < fsize; i++) {
		to[i] = bobj->bytes[i];
	}
	to[i] = '\0';	/* put null terminator at end */
}

struct object *
primitive(int primitiveNumber, struct object *args, int *failed)
{
	struct object *returnedValue = nilObject;
	int i, j;
	FILE *fp;
	char *p;
	struct byteObject *stringReturn;
	char nameBuffer[80], modeBuffer[80];

	*failed = 0;
	switch(primitiveNumber) {
	case 100: 	/* open a file */
		getUnixString(nameBuffer, 80, args->data[0]);
		getUnixString(modeBuffer, 10, args->data[1]);
		fp = fopen(nameBuffer, modeBuffer);
		if (fp != NULL) {
			for (i = 0; i < FILEMAX; ++i) {
				if (filePointers[i] == NULL) {
					break;
				}
			}
			if (i >= FILEMAX) {
				sysError("too many open files", 0);
				fclose(fp);
				*failed = 1;
			} else {
				returnedValue = newInteger(i);
				filePointers[i] = fp;
			}
		} else {
			*failed = 1;
		}
		break;

	case 101:	/* read a single character from a file */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}
		i = fgetc(fp);
		if (i != EOF) {
			returnedValue = newInteger(i);
		}
		break;

	case 102:	/* write a single character to a file */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}
		fputc(integerValue(args->data[1]), fp);
		break;

	case 103:	/* close file */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}
		fclose(fp);
		filePointers[i] = NULL;
		break;

	case 104:	/* file out image */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}
		fileOut(fp);
		break;

	case 105:	/* edit a string */
			/* first get the name of a temp file */
		sprintf(nameBuffer, "%s/lsteditXXXXXX", tmpdir);
		mktemp(nameBuffer);
			/* copy string to file */
		fp = fopen(nameBuffer, "w");
		if (fp == NULL) 
			sysError("cannot open temp edit file", 0);
		j = SIZE(args->data[0]);
		p = ((struct byteObject *) args->data[0])->bytes;
		for (i = 0; i < j; i++)
			fputc(*p++, fp);
		fputc('\n', fp);
		fclose(fp);
			/* edit string */
		strcpy(modeBuffer,"vi ");
		strcat(modeBuffer,nameBuffer);
		system(modeBuffer);
			/* copy back to new string */
		fp = fopen(nameBuffer, "r");
		if (fp == NULL) 
			sysError("cannot open temp edit file", 0);
			/* get length of file */
		fseek(fp, 0, 2);
		j = (int) ftell(fp);
		stringReturn = gcialloc(j);
		returnedValue = (struct object *)stringReturn;
		returnedValue->class = args->data[0]->class;
			/* reset to beginning, and read values */
		fseek(fp, 0, 0);
		for (i = 0; i < j; i++)
			stringReturn->bytes[i] = fgetc(fp);
			/* now clean up files */
		fclose(fp);
		unlink(nameBuffer);
		break;

	case 106:	/* Read into ByteArray */
		/* File descriptor */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}

		/* Make sure we're populating an array of bytes */
		returnedValue = args->data[1];
		if ((returnedValue->size & FLAG_BIN) == 0) {
			*failed = 1;
			break;
		}

		/* Sanity check on I/O count */
		i = integerValue(args->data[2]);
		if ((i < 0) || (i > SIZE(returnedValue))) {
			*failed = 1;
			break;
		}

		/* Do the I/O */
		i = fread(bytePtr(returnedValue), sizeof(char), i, fp);
		if (i < 0) {
			*failed = 1;
			break;
		}
		returnedValue = newInteger(i);
		break;

	case 107:	/* Write from ByteArray */
		/* File descriptor */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}

		/* Make sure we're writing an array of bytes */
		returnedValue = args->data[1];
		if ((returnedValue->size & FLAG_BIN) == 0) {
			*failed = 1;
			break;
		}

		/* Sanity check on I/O count */
		i = integerValue(args->data[2]);
		if ((i < 0) || (i > SIZE(returnedValue))) {
			*failed = 1;
			break;
		}

		/* Do the I/O */
		i = fwrite(bytePtr(returnedValue), sizeof(char), i, fp);
		if (i < 0) {
			*failed = 1;
			break;
		}
		returnedValue = newInteger(i);
		break;

	case 108:	/* Seek to file position */
		/* File descriptor */
		i = integerValue(args->data[0]);
		if ((i < 0) || (i >= FILEMAX) || !(fp = filePointers[i])) {
			*failed = 1;
			break;
		}

		/* File position */
		i = integerValue(args->data[1]);
		if ((i < 0) || ((i = fseek(fp, i, SEEK_SET)) < 0)) {
			*failed = 1;
			break;
		}

		/* Return position as our value */
		returnedValue = newInteger(i);
		break;

	default:
		sysError("unknown primitive", primitiveNumber);
	}
	return(returnedValue);
}

```

```c lst4/src/source/memory.c
/*
	Little Smalltalk memory management
	Written by Tim Budd, budd@cs.orst.edu
	All rights reserved, no guarantees given whatsoever.
	May be freely redistributed if not for profit.

	Uses baker two-space garbage collection algorithm
*/

# include <stdio.h>
# include <stdlib.h>
# include "memory.h"
# include "globs.h"

extern int debugging;	/* true if we are debugging */

/*
	static memory space -- never recovered
*/
static struct object *staticBase, *staticTop, *staticPointer;

/*
	dynamic (managed) memory space
	recovered using garbage collection
*/

static struct object *spaceOne, *spaceTwo;
static int spaceSize;

struct object *memoryBase, *memoryPointer, *memoryTop;

static int inSpaceOne;
static struct object *oldBase, *oldTop;

/*
	roots for memory access
	used as bases for garbage collection algorithm
*/
struct object *rootStack[ROOTSTACKLIMIT];
int rootTop = 0;
#define STATICROOTLIMIT (200)
static struct object **staticRoots[STATICROOTLIMIT];
static int staticRootTop = 0;

/*
	test routine to see if a pointer is in dynamic memory
	area or not
*/

int
isDynamicMemory(struct object * x)
{
	return ((x >= spaceOne) && (x <= (spaceOne + spaceSize))) ||
		((x >= spaceTwo) && (x <= (spaceTwo + spaceSize)));
}

/*
	gcinit -- initialize the memory management system
*/
void
gcinit(int staticsz, int dynamicsz)
{
		/* allocate the memory areas */
	staticBase = (struct object *)
		malloc(staticsz * sizeof(struct object));
	spaceOne = (struct object *)
		malloc(dynamicsz * sizeof(struct object));
	spaceTwo = (struct object *)
		malloc(dynamicsz * sizeof(struct object));
	if ((staticBase == 0) || (spaceOne == 0) || (spaceTwo == 0))
		sysError("not enough memory for space allocations\n", 0);

	staticTop = staticBase + staticsz;
	staticPointer = staticTop;

	spaceSize = dynamicsz;
	memoryBase = spaceOne;
	memoryPointer = memoryBase + spaceSize;
	if (debugging) {
		printf("space one 0x%x, top 0x%x,"
				" space two 0x%x , top 0x%x\n",
			(uint)spaceOne, (uint)(spaceOne + spaceSize),
			(uint)spaceTwo, (uint)(spaceTwo + spaceSize));
	}
	inSpaceOne = 1;
}

/*
	gc_move is the heart of the garbage collection algorithm.
	It takes as argument a pointer to a value in the old space,
	and moves it, and everything it points to, into the new space
	The returned value is the address in the new space.
*/
struct mobject {
	uint size;
	struct mobject *data[0];
};

static struct object *
gc_move(struct mobject * ptr)
{
	struct mobject *old_address = ptr, *previous_object = 0,
		*new_address = 0, *replacement  = 0;
	int sz;

	while (1) {

		/*
		 * part 1.  Walking down the tree
		 * keep stacking objects to be moved until we find
		 * one that we can handle
		 */
		for (;;) {
			/*
			 * SmallInt's are not proper memory pointers,
			 * so catch them first.  Their "object pointer"
			 * value can be used as-is in the new space.
			 */
			if (IS_SMALLINT(old_address)) {
				replacement = old_address;
				old_address = previous_object;
				break;

			/*
			 * If we find a pointer in the current space
			 * to the new space (other than indirections) then
			 * something is very wrong
			 */
			} else if ((old_address >=
			 (struct mobject *) memoryBase)
			 && (old_address <= (struct mobject *) memoryTop)) {
				sysError(
				 "GC invariant failure -- address in new space",
					(unsigned int)old_address);

			/* else see if not  in old space */
			} else if ((old_address < (struct mobject *) oldBase) ||
			 (old_address > (struct mobject *) oldTop)) {
				replacement = old_address;
				old_address = previous_object;
				break;

			/* else see if already forwarded */
			} else if (old_address->size & FLAG_GCDONE)  {
				if (old_address->size & FLAG_BIN) {
					replacement = old_address->data[0];
				} else {
					sz = SIZE(old_address);
					replacement = old_address->data[sz];
				}
				old_address = previous_object;
				break;

			/* else see if binary object */
			} else if (old_address->size & FLAG_BIN) {
				int isz;

				isz = SIZE(old_address);
				sz = (isz + BytesPerWord - 1)/BytesPerWord;
				memoryPointer = WORDSDOWN(memoryPointer,
					sz + 2);
				new_address = (struct mobject *)memoryPointer;
				SETSIZE(new_address, isz);
				new_address->size |= FLAG_BIN;
				while (sz) {
					new_address->data[sz] =
						old_address->data[sz];
					sz--;
				}
				old_address->size |= FLAG_GCDONE;
				new_address->data[0] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[0];
				previous_object->data[0] = new_address;
				/* now go chase down class pointer */

			/* must be non-binary object */
			} else  {
				sz = SIZE(old_address);
				memoryPointer = WORDSDOWN(memoryPointer,
					sz + 2);
				new_address = (struct mobject *)memoryPointer;
				SETSIZE(new_address, sz);
				old_address->size |= FLAG_GCDONE;
				new_address->data[sz] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[sz];
				previous_object->data[sz] = new_address;
			}
		}

		/*
		 * part 2.  Fix up pointers,
		 * move back up tree as long as possible
		 * old_address points to an object in the old space,
		 * which in turns points to an object in the new space,
		 * which holds a pointer that is now to be replaced.
		 * the value in replacement is the new value
		 */
		for (;;) {
			/* backed out entirely */
			if (old_address == 0) {
				return (struct object *) replacement;
			}

			/* case 1, binary or last value */
			if ((old_address->size & FLAG_BIN) ||
			 (SIZE(old_address) == 0)) {

				/* fix up class pointer */
				new_address = old_address->data[0];
				previous_object = new_address->data[0];
				new_address->data[0] = replacement;
				old_address->data[0] = new_address;
				replacement = new_address;
				old_address = previous_object;
			} else {
				sz = SIZE(old_address);
				new_address = old_address->data[sz];
				previous_object = new_address->data[sz];
				new_address->data[sz] = replacement;
				sz--;

				/*
				 * quick cheat for recovering zero fields
				 */
				while (sz && (old_address->data[sz] == 0)) {
					new_address->data[sz--] = 0;
				}

				SETSIZE(old_address, sz);
				old_address->size |= FLAG_GCDONE;
				new_address->data[sz] = previous_object;
				previous_object = old_address;
				old_address = old_address->data[sz];
				previous_object->data[sz] = new_address;
				break; /* go track down this value */
			}
		}
	}
}

/*
	gcollect -- garbage collection entry point
*/
extern int gccount;
struct object *
gcollect(int sz)
{
	int i;

	gccount++;

	/* first change spaces */
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

	/* then do the collection */
	for (i = 0; i < rootTop; i++) {
		rootStack[i] = gc_move((struct mobject *) rootStack[i]);
	}
	for (i = 0; i < staticRootTop; i++) {
		(* staticRoots[i]) =  gc_move((struct mobject *)
			*staticRoots[i]);
	}

	flushCache();

	/* then see if there is room for allocation */
	memoryPointer = WORDSDOWN(memoryPointer, sz + 2);
	if (memoryPointer < memoryBase) {
		sysError("insufficient memory after garbage collection", sz);
	}
	SETSIZE(memoryPointer, sz);
	return(memoryPointer);
}

/*
	static allocation -- tries to allocate values in an area
	that will not be subject to garbage collection
*/

struct object *
staticAllocate(int sz)
{
	staticPointer = WORDSDOWN(staticPointer, sz + 2);
	if (staticPointer < staticBase) {
		sysError("insufficient static memory", 0);
	}
	SETSIZE(staticPointer, sz);
	return(staticPointer);
}

struct object *
staticIAllocate(int sz)
{
	int trueSize;
	struct object * result;

	trueSize = (sz + BytesPerWord - 1) / BytesPerWord;
	result = staticAllocate(trueSize);
	SETSIZE(result, sz);
	result->size |= FLAG_BIN;
	return result;
}

/*
	if definition is not in-lined, here  is what it should be
*/
#ifndef gcalloc
struct object *
gcalloc(int sz)
{
	struct object * result;

	memoryPointer = WORDSDOWN(memoryPointer, sz + 2);
	if (memoryPointer < memoryBase) {
		return gcollect(sz);
	}
	SETSIZE(memoryPointer, sz);
	return(memoryPointer);
}
# endif

struct object *
gcialloc(int sz)
{
	int trueSize;
	struct object * result;

	trueSize = (sz + BytesPerWord - 1) / BytesPerWord;
	result = gcalloc(trueSize);
	SETSIZE(result, sz);
	result->size |= FLAG_BIN;
	return result;
}

/*
	File in and file out of Smalltalk images
*/

static int indirtop = 0;
static struct object **indirArray;

static unsigned int
readWord(FILE * fp)
{
	int i;

	i = fgetc(fp);
	if (i == EOF) {
		sysError("unexpected end of file reading image file", 0);
	}
	if (i == 255) {
		return 255 + readWord(fp);
	} else {
		return i;
	}
}

static struct object *
objectRead(FILE * fp)
{
	int type, size, i;
	struct object *newObj = 0;
	struct byteObject *bnewObj;

	type = readWord(fp);

	switch (type) {
		case 0:	/* nil obj */
			sysError("read in a null object", (int)newObj);

		case 1:	/* ordinary object */
			size = readWord(fp);
			newObj = staticAllocate(size);
			indirArray[indirtop++] = newObj;
			newObj->class = objectRead(fp);
			for (i = 0; i < size; i++) {
				newObj->data[i] = objectRead(fp);
				}
			break;

		case 2: /* integer */
			{
			int val;

			(void)fread(&val, sizeof(val), 1, fp);
			newObj = newInteger(val);
			}
			break;

		case 3:	/* byte arrays */
			size = readWord(fp);
			newObj = staticIAllocate(size);
			indirArray[indirtop++] = newObj;
			bnewObj = (struct byteObject *) newObj;
			for (i = 0; i < size; i++) {
				type = readWord(fp);
				bnewObj->bytes[i] = type;
				}
			bnewObj->class = objectRead(fp);
			break;

		case 4:	/* previous object */
			size = readWord(fp);
			newObj = indirArray[size];
			break;

		case 5:	/* object 0 (nil object) */
			newObj = indirArray[0];
			break;
		}
	return newObj;
}


int
fileIn(FILE * fp)
{
	int i;

	/* use the currently unused space for the indir pointers */
	if (inSpaceOne) {
		indirArray = (struct object * *) spaceTwo;
	} else {
		indirArray = (struct object * *) spaceOne;
	}
	indirtop = 0;

	/* read in the method from the image file */
	nilObject = objectRead(fp);
	trueObject = objectRead(fp);
	falseObject = objectRead(fp);
	globalsObject = objectRead(fp);
	SmallIntClass = objectRead(fp);
	IntegerClass = objectRead(fp);
	ArrayClass = objectRead(fp);
	BlockClass = objectRead(fp);
	ContextClass = objectRead(fp);
	initialMethod = objectRead(fp);
	for (i = 0; i < 3; i++) {
		binaryMessages[i] = objectRead(fp);
	}
	badMethodSym = objectRead(fp);

	/* clean up after ourselves */
	bzero((char *) indirArray, spaceSize * sizeof(struct object));
	return indirtop;
}

static void
writeWord(FILE * fp, int i)
{
	if (i < 0) {
		sysError("trying to write out negative value", i);
	}
	if (i >= 255) {
		fputc(255, fp);
		writeWord(fp, i - 255);
	} else {
		fputc(i, fp);
	}
}

static void
objectWrite(FILE *fp, struct object * obj)
{
	int i, size;

	if (obj == 0) {
		sysError("writing out a null object", (int)obj);
	}

	if (IS_SMALLINT(obj)) { /* SmallInt */
		int val;

		writeWord(fp, 2);
		val = integerValue(obj);
		(void)fwrite(&val, sizeof(val), 1, fp);
		return;
	}

	/* see if already written */
	for (i = 0; i < indirtop; i++) {
		if (obj == indirArray[i]) {
			if (i == 0) {
				writeWord(fp, 5);
			} else {
				writeWord(fp, 4);
				writeWord(fp, i);
			}
			return;
		}
	}

	/* not written, do it now */
	indirArray[indirtop++] = obj;

	/* byte objects */
	if (obj->size & FLAG_BIN) {
		struct byteObject * bobj = (struct byteObject *) obj;
		size = SIZE(obj);
		writeWord(fp, 3);
		writeWord(fp, size);
		for (i = 0; i < size; i++)
			writeWord(fp, bobj->bytes[i]);
		objectWrite(fp, obj->class);
		return;
	}

	/* ordinary objects */
	size = SIZE(obj);
	writeWord(fp, 1);
	writeWord(fp, size);
	objectWrite(fp, obj->class);
	for (i = 0; i < size; i++) {
		objectWrite(fp, obj->data[i]);
	}
}

int
fileOut(FILE * fp)
{
	int i;

	/* use the currently unused space for the indir pointers */
	if (inSpaceOne) {
		indirArray = (struct object * *) spaceTwo;
	} else {
		indirArray = (struct object * *) spaceOne;
	}
	indirtop = 0;

	/* write out the roots of the image file */
	objectWrite(fp, nilObject);
	objectWrite(fp, trueObject);
	objectWrite(fp, falseObject);
	objectWrite(fp, globalsObject);
	objectWrite(fp, SmallIntClass);
	objectWrite(fp, IntegerClass);
	objectWrite(fp, ArrayClass);
	objectWrite(fp, BlockClass);
	objectWrite(fp, ContextClass);
	objectWrite(fp, initialMethod);
	for (i = 0; i < 3; i++) {
		objectWrite(fp, binaryMessages[i]);
	}
	objectWrite(fp, badMethodSym);
	printf("%d objects written in image\n", indirtop);

	/* clean up after ourselves */
	bzero((char *) indirArray, spaceSize * sizeof(struct object));
	return indirtop;
}

/*
 * addStaticRoot()
 *	Add another object root off a static object
 *
 * Static objects, in general, do not get garbage collected.  When
 * a static object is discovered adding a reference to a non-static
 * object, we link on the reference to our staticRoot table so we can
 * give it proper treatment during garbage collection.
 */
void
addStaticRoot(struct object **objp)
{
	int i;

	for (i = 0; i < staticRootTop; ++i) {
		if (objp == staticRoots[i]) {
			return;
		}
	}
	if (staticRootTop >= STATICROOTLIMIT) {
		sysError("addStaticRoot: too many static references",
			(unsigned int)objp);
	}
	staticRoots[staticRootTop++] = objp;
}

/*
 * map()
 *	Fix an OOP if needed, based on values to be exchanged
 */
static void
map(struct object **oop, struct object *a1, struct object *a2, int size)
{
	int x;
	struct object *oo = *oop;

	for (x = 0; x < size; ++x) {
		if (a1->data[x] == oo) {
			*oop = a2->data[x];
			return;
		}
		if (a2->data[x] == oo) {
			*oop = a1->data[x];
			return;
		}
	}
}

/*
 * walk()
 *	Traverse an object space
 */
static void
walk(struct object *base, struct object *top,
	struct object *array1, struct object *array2, uint size)
{
	struct object *op, *opnext;
	uint x, sz;

	for (op = base; op < top; op = opnext) {
		/*
		 * Re-map the class pointer, in case that's the
		 * object which has been remapped.
		 */
		map(&op->class, array1, array2, size);

		/*
		 * Skip our argument arrays, since otherwise things
		 * get rather circular.
		 */
		sz = SIZE(op);
		if ((op == array1) || (op == array2)) {
			opnext = WORDSUP(op, sz + 2);
			continue;
		}

		/*
		 * Don't have to worry about instance variables
		 * if it's a binary format.
		 */
		if (op->size & FLAG_BIN) {
			uint trueSize;

			/*
			 * Skip size/class, and enough words to
			 * contain the binary bytes.
			 */
			trueSize = (sz + BytesPerWord - 1) / BytesPerWord;
			opnext = WORDSUP(op, trueSize + 2);
			continue;
		}

		/*
		 * For each instance variable slot, fix up the pointer
		 * if needed.
		 */
		for (x = 0; x < sz; ++x) {
			map(&op->data[x], array1, array2, size);
		}

		/*
		 * Walk past this object
		 */
		opnext = WORDSUP(op, sz + 2);
	}
}

/*
 * exchangeObjects()
 *	Bulk exchange of object identities
 *
 * For each index to array1/array2, all references in current object
 * memory are modified so that references to the object in array1[]
 * become references to the corresponding object in array2[].  References
 * to the object in array2[] similarly become references to the
 * object in array1[].
 */
void
exchangeObjects(struct object *array1, struct object *array2, uint size)
{
	uint x;

	/*
	 * Convert our memory spaces
	 */
	walk(memoryPointer, memoryTop, array1, array2, size);
	walk(staticPointer, staticTop, array1, array2, size);

	/*
	 * Fix up the root pointers, too
	 */
	for (x = 0; x < rootTop; x++) {
		map(&rootStack[x], array1, array2, size);
	}
	for (x = 0; x < staticRootTop; x++) {
		map(staticRoots[x], array1, array2, size);
	}
}

```

```h lst4/src/source/memory.h
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

struct object {
	uint size;
	struct object *class;
	struct object *data[0];
};

/*
	byte objects are used to represent strings and symbols
		bytes per word must be correct
*/

struct byteObject {
	uint size;
	struct object *class;
	unsigned char bytes[0];
};

# define BytesPerWord 4 //maybe 2?
# define bytePtr(x) (((struct byteObject *) x)->bytes)
#define WORDSUP(ptr, amt) ((struct object *)(((char *)(ptr)) + \
	((amt) * BytesPerWord)))
#define WORDSDOWN(ptr, amt) WORDSUP(ptr, 0 - (amt))

/*
 * SmallInt objects are used to represent short integers.  They are
 * encoded as 31 bits, signed, with the low bit set to 1.  This
 * distinguishes them from all other objects, which are longword
 * aligned and are proper C memory pointers.
 */
 // this may need to be modified to 24 bits, ala Agon ADL
 // actually maybe 16?
#include <limits.h>

#define IS_SMALLINT(x) ((((int)(x)) & 0x01) != 0)
#define FITS_SMALLINT(x) ((((int)(x)) >= INT_MIN/2) && \
	(((int)(x)) <= INT_MAX/2))
#define CLASS(x) (IS_SMALLINT(x) ? SmallIntClass : ((x)->class))
#define integerValue(x) (((int)(x)) >> 1)
#define newInteger(x) ((struct object *)((((int)(x)) << 1) | 0x01))

/*
 * The "size" field is the top 30 bits; the bottom two are flags
 */
 // here this will need to be 24 bits as well.
#define SIZE(op) ((op)->size >> 2)
#define SETSIZE(op, val) ((op)->size = ((val) << 2))
#define FLAG_GCDONE (0x01)
#define FLAG_BIN (0x02)

/*
	memoryBase holds the pointer to the current space,
	memoryPointer is the pointer into this space.
	To allocate, decrement memoryPointer by the correct amount.
	If the result is less than memoryBase, then garbage collection
	must take place

*/

extern struct object *memoryPointer, *memoryBase;

/*
	roots for the memory space
	these are traced down during memory management
	rootStack is the dynamic stack
	staticRoots are values in static memory that point to
	dynamic values
*/
# define ROOTSTACKLIMIT 50
extern struct object *rootStack[];
extern int rootTop;
extern void addStaticRoot(struct object **);

/*
	The following are roots for the file out
*/

extern struct object *nilObject, *trueObject,
	*falseObject, *SmallIntClass, *ArrayClass, *BlockClass,
	*ContextClass, *globalsObject, *initialMethod,
	*binaryMessages[3], *IntegerClass, *badMethodSym;

/*
	entry points
*/

extern void gcinit(int, int);
extern struct object *gcollect(int), *staticAllocate(int),
	*staticIAllocate(int), *gcialloc(int);
extern void exchangeObjects(struct object *, struct object *, uint);

extern int isDynamicMemory(struct object *);

#define gcalloc(sz) (((memoryPointer = WORDSDOWN(memoryPointer, (sz) + 2)) < \
	memoryBase) ? gcollect(sz) : \
	(SETSIZE(memoryPointer, (sz)), memoryPointer))

#ifndef gcalloc
extern struct object *gcalloc(int);
#endif

```
</directories>
</context>

So I would like to simplify this Little Smalltalk project into imeplmentation phases. I want to port it to a Agon Light, using the agondev C compiler. BUT I'd like to have a series of phases that shows how to port something like this by cross-compiling the very basics, and then adding more sophisticated features. 

## Agent



<think>
I'm being asked to create a phased approach for porting the Little Smalltalk project to an Agon Light platform using the agondev C compiler. Let me think about how to break this down into manageable implementation phases, starting with a minimal core system and progressively adding features.

First, let me understand what we're dealing with:

1. **Source Files Structure**:
   - Main Smalltalk interpreter (`interp.c`, `interp.h`)
   - Memory management (`memory.c`, `memory.h`)
   - Main program entry and primitives (`main.c`)
   - Globals and utility functions (`globs.h`)
   - Image building tools (`ImageBuilder/imageBuilder.c`, etc.)

2. **The Agon Light Platform**:
   - Z80-based retro computer with limited resources
   - agondev C compiler will have constraints compared to a full desktop C compiler
   - Memory management and byte ordering might differ

3. **Key Challenges**:
   - Memory constraints
   - Potential issues with 32-bit values on a smaller architecture
   - File system differences
   - Console I/O differences

Let me think about how to break this into logical phases:

### Phase 1: Minimal Core System
- Simplify the memory model (potentially reduce 32-bit values to 16-bit)
- Implement bare minimum interpreter functionality
- Focus on basic bytecode execution without garbage collection
- Simple REPL without files or complex I/O

### Phase 2: Basic Memory Management
- Implement simplified garbage collection
- Add support for basic Smalltalk objects
- Basic primitive operations

### Phase 3: First Working Image
- Create a simple image format
- Implement basic image loading
- Support minimal class hierarchy

### Phase 4: Enhanced Runtime
- More complex primitives
- Complete garbage collection
- File operations

### Phase 5: Development Tools
- Image builder
- Debugging facilities
- More complete class library

Let me flesh these out in more detail with specific implementation steps:
</think>

# Little Smalltalk Agon Light Implementation Plan

I'll outline a phased approach for porting Little Smalltalk to the Agon Light using the agondev C compiler. This will allow you to start with a minimal system and progressively enhance it as you validate each component.

## Phase 0: Understanding and Analysis

Before writing any code, I recommend:

1. **Architecture Assessment**
   - Understand Agon Light's Z80 architecture constraints (16-bit vs 32-bit operations)
   - Memory layout and limitations (how much RAM is available?)
   - eZ80 specifics that might affect C code performance

2. **Compiler Capabilities**
   - Test agondev C compiler's support for dynamic memory allocation
   - Determine struct alignment rules and byte ordering
   - Check for any compiler flags needed for optimization

## Phase 1: Minimal Interpreter Core

This phase focuses on just getting the core VM working:

1. **Simplify Memory Model**
   - Modify `memory.h` to use 16-bit values instead of 32-bit where appropriate
   - Adjust `IS_SMALLINT` and related macros to work with 16-bit integers
   - Reduce object header size to fit Z80 architecture

2. **Basic Bytecode Interpreter**
   - Implement a stripped-down version of `interp.c` with only essential bytecodes
   - Start with only: PushConstant, PushLiteral, SendMessage, DoSpecial
   - Skip complex primitives for now

3. **Simple Object Representation**
   - Create minimal class structure for: Object, SmallInt, String
   - Implement bare minimum method lookup

4. **Testing Framework**
   - Create simple manual tests that can verify bytecode execution

```c
// Example simplified memory.h for Agon
typedef unsigned int uint;  // 16-bit on Z80
typedef unsigned char byte;

#define BytesPerWord 2     // Adjusted for 16-bit architecture
#define SMALLINT_MASK 0x01
#define IS_SMALLINT(x) ((((int)(x)) & SMALLINT_MASK) != 0)
#define integerValue(x) (((int)(x)) >> 1)
#define newInteger(x) ((struct object *)((((int)(x)) << 1) | SMALLINT_MASK))
```

## Phase 2: Basic Memory Management

Once the core interpreter is working:

1. **Static Memory Allocation**
   - Implement `staticAllocate` function without garbage collection
   - Create fixed-size memory pools for objects

2. **Simple Garbage Collection**
   - Simplify Baker's algorithm for two-space collection 
   - Implement basic reference counting as an alternative if full GC is too complex

3. **Basic Primitives**
   - Implement arithmetic primitives (add, subtract, etc.)
   - Basic comparison operators
   - Simple I/O (putchar, getchar)

4. **Test Harness**
   - Create a simple test loop that exercises memory management

## Phase 3: Minimal Image System

Now we need to be able to load a Smalltalk environment:

1. **Simplified Image Format**
   - Design a compact image format suitable for limited memory
   - Implement basic image loading functions

2. **Bootstrap Classes**
   - Create minimal image with: Object, SmallInt, String, Array, Dictionary
   - Implement just enough methods to start the system

3. **Simple REPL**
   - Create a basic read-eval-print loop
   - Allow entering simple expressions

```c
// Example of a simplified imageReader function
struct object* loadMinimalImage(const char* filename) {
    FILE* f = fopen(filename, "rb");
    if (!f) return NULL;
    
    // Read essential objects (nil, true, false)
    nilObject = readObject(f);
    trueObject = readObject(f);
    falseObject = readObject(f);
    
    // Read base classes
    SmallIntClass = readObject(f);
    StringClass = readObject(f);
    
    // Return initial method to execute
    return readObject(f);
}
```

## Phase 4: Enhanced Runtime Features

With a working minimal system, we can add more capabilities:

1. **Full Bytecode Support**
   - Implement remaining bytecodes from `interp.c`
   - Add method caching for performance

2. **Extended Primitives**
   - File I/O primitives adapted for Agon
   - Graphics primitives if Agon supports them
   - System access primitives

3. **Complete Memory Management**
   - Finalize garbage collection algorithm
   - Add memory compaction if needed

4. **Enhanced REPL**
   - Support for multiline input
   - Basic error handling and debugging

## Phase 5: Development Tools

Finally, add tools that make development easier:

1. **Image Builder**
   - Port the `ImageBuilder` tool to create custom images
   - Add ability to save modified images

2. **Debugging Facilities**
   - Implement stack tracing
   - Add breakpoint support
   - Basic inspector for objects

3. **Class Library Expansion**
   - Port additional classes from the original codebase
   - Add Agon-specific classes for hardware access

4. **Performance Optimization**
   - Identify bottlenecks and optimize critical paths
   - Consider assembly language for key routines

## Key Adaptation Areas

Throughout all phases, pay special attention to:

1. **Word Size Adjustments**
   - Change 32-bit integers to 16-bit where appropriate
   - Be careful with pointer arithmetic

2. **Memory Limitations**
   - Reduce object size overhead
   - Consider using packed structures

3. **I/O Differences**
   - Adapt file operations to Agon filesystem
   - Implement console I/O using Agon's API

4. **Compiler Quirks**
   - Watch for agondev C compiler limitations
   - Avoid complex constructs that might not be supported

This phased approach lets you validate each component incrementally, ensuring you have a working system at each step. It also makes debugging easier as you'll know which phase introduced any issues.

Would you like me to dive deeper into any specific phase or aspect of this implementation plan?

