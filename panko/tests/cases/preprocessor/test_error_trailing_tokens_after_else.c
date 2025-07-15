#if true
#else a
#endif

#if true
// Should not error because it’s required that skipped directives are not
// parsed, only looking at directive names to determine nesting is allowed.
#elifdef +a
// ditto
#else a
#endif

#if false
#else a
#endif

int main() {}
