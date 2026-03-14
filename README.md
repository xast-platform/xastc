# xastc

**xastc** is an official Xast language compiler, written in Haskell, which compiles to efficient C.

## ToDo List:
- [x] Parsing
- [x] Import resolve
- [x] Declaration resolve
- [ ] Compile-time modes for systems (Strict, Safe, Dynamic);
- [ ] Name resolution;
- [ ] Type checking;
- [ ] IR generation;
- [ ] Pattern matching with `match`;
- [ ] Compile to C using Flecs;
- [ ] Bundle `tcc` (Tiny C Compiler) for hot reload compiles;
- [ ] Extern types (to use with extern functions);
- [ ] Suggest symbols to import;