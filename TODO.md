# TODO List for LnMessagepack

## Priority 1 (Critical/Blocking)
1. **Implement float encoding/decoding**   Currently panics with "float encoding not implemented"
2. **Remove unsafe operations**   Replace `get!` and `String.fromUTF8!` with safe alternatives that return `Option`/`Except`
3. **Add comprehensive error handling**   Replace generic error strings with structured error types
4. **Fix potential buffer overflows**   Add bounds checking for all byte array operations

## Priority 2 (Important for Production)
5. **Implement streaming parser**   Avoid loading entire payload into memory for large data
6. **Add memory efficient encoding**   Replace array concatenation with buffer based approach
7. **Implement all timestamp formats**   Support 32 bit, 64 bit, and 96 bit timestamp extensions per spec
8. **Add input validation**   Validate UTF 8 strings, check for malformed data, prevent denial of service
9. **Performance optimization**   Profile and optimize hot paths, consider zero copy parsing where possible

## Priority 3 (Quality of Life)
10. **Improve error messages**   Include byte offsets, expected vs actual values, context information
11. **Add fuzzing tests**   Generate random inputs to catch edge cases and crashes
12. **Implement remaining MessagePack types**   Add support for any missing format codes
13. **Add benchmarks comparison**   Compare against other MessagePack implementations
14. **Memory usage profiling**   Track and optimize memory allocation patterns

## Priority 4 (Nice to Have)
15. **Add schema validation**   Optional runtime type checking for decoded values
16. **Implement custom extension types**   Generic framework for user defined extensions beyond timestamps
17. **Add pretty printing improvements**   Better formatting for nested structures, truncation for large data
18. **Create more comprehensive examples**   Real world usage patterns, integration examples
19. **Add documentation generation**   API docs, usage guides, performance characteristics
20. **Consider async/streaming API**   For integration with IO heavy applications

## Priority 5 (Future Enhancements)
21. **Cross language compatibility testing**   Verify interop with other MessagePack implementations
22. **Add compression support**   Optional compression layer for large payloads
23. **Implement map key constraints**   Enforce string only keys or other application specific rules
24. **Add serialization versioning**   Handle schema evolution gracefully
25. **Create language bindings**   C FFI or other language interfaces if needed
