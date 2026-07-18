// [[known-bug: stringising `__VA_ARGS__` is not checked for varargs-ness]]

// [[compile-error: `__VA_ARGS__` outside of variadic macro]]
#define M(a, b) #__VA_ARGS__

// [[compile-error: `__VA_OPT__` outside of variadic macro]]
#define M2(a, b) #__VA_OPT__(42)
