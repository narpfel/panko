#if (-0x1'0000'0000'0000'0000 + 1) / 0
#endif

#if false && (-0x1'0000'0000'0000'0000 + 1) / 0
#endif

#if (-0x1'ffff'ffff'ffff'ffff - 1) % -1
#endif

int main() {}
