
now create this component.
**Simplify Memory Model**
   - Modify `memory.h` to use 16-bit values instead of 32-bit where appropriate
   - Adjust `IS_SMALLINT` and related macros to work with 16-bit integers
   - Reduce object header size to fit Z80 architecture

 **Simple Object Representation**
   - Create minimal class structure for: Object, SmallInt, String
   - Implement bare minimum method lookup

 **Testing Framework**
   - Create simple manual tests that can verify bytecode execution

This simplified implementation gives you:

1. A 16-bit optimized memory system with compact object headers
2. Basic garbage collection that works on limited memory
3. Core object types needed for a minimal Smalltalk system
4. A simple testing framework to verify things are working

Key adaptations for Z80 architecture:
- SmallInts are 15-bit (+/- 16383) instead of 31-bit
- Object headers reduced to 4 bytes total (2-byte size, 2-byte class pointer)
- Stack sizes and temporary storage reduced for memory conservation
- Space sizes are configurable but default much smaller

This gives you a solid foundation to build upon. You can first make sure these basics work, then add primitives for I/O and other essential operations specific to the Agon Light.
