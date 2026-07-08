// [[known-bug: inline functions with external linkage must not define modifiable static objects]]

// [[compile-error: inline function `g` with external linkage contains a definition of a modifiable object with static storage duration]]
inline typeof((int){0}) g() {
    return 0;
}
