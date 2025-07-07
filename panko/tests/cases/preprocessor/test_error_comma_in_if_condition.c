#if 1, 2
#endif

#if (1, 2)
#endif

#if 4 * (1, 2)
#endif

// allowed in unevaluated subexpressions
#if false && (1, 2)
#endif

// allowed in unevaluated subexpressions
#if 1 ? 2 : (3, 4)
#endif

int main() {}
