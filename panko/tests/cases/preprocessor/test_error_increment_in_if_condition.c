#if 1++
#endif

#if ++2
#endif

#if 2 + (3--)
#endif

#if 2 + (--3)
#endif

#if false && (5++)
#endif

#if false && (--5)
#endif

int main() {}
