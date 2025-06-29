#if 0x7fff'ffff'ffff'ffff + 1
#endif

#if -(-0x7fff'ffff'ffff'ffff - 1)
#endif

// test that errors from both branches are shown
#if (0x7fff'ffff'ffff'ffff + 1) && -(-0x7fff'ffff'ffff'ffff - 1)
#endif

// test that unevaluated branches don’t emit errors
#if false && 0x7fff'ffff'ffff'ffff + 1
#endif

// test that unevaluated branch of ternary doesn’t emit errors
#if true ? 0x7fff'ffff'ffff'ffff * 42 : 0x7fff'ffff'ffff'ffff * 2
#endif

#if 0x7fff'ffff'ffff'ffff * 42 ? 0 : 1
#endif

int main() {}
