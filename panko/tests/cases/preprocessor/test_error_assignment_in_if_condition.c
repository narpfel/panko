#if 1 = 2 + 42
#endif

#if 2 + (3 = 4)
#endif

#if false && (5 = 6)
#endif

#if 1 += 2 + 42
#endif

#if 2 + (3 -= 4)
#endif

#if false && (5 *= 6)
#endif

int main() {}
