#if 42 << -1
#endif

#if -42 << -1
#endif

#if (42 << -1) << 64
#endif

#if 1 << 63
int should_error;
#endif

#if 0xffffu << 63
int okay;
#endif

#if 0 << 64u
int should_error;
#endif

int main() {}
