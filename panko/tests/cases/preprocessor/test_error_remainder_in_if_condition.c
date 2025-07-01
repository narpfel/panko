#if 42 % 0
#endif

#if (42 % 0) % 0
#endif

// TODO: this is accepted by GCC, clang and CompCert, but should be UB?
#if (-0x7fff'ffff'ffff'ffff - 1) % -1
#endif

int main() {}
