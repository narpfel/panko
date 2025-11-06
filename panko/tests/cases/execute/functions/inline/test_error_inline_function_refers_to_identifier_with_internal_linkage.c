// [[known-bug]]

static int a;

// [[compile-error: inline function `f` with external linkage refers to identifier `a` with internal linkage]]
inline typeof(a) f() {
    return 0;
}
