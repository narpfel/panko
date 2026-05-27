int a[42];
int b[sizeof 1];
int c[_Lengthof b];
int d[sizeof(typeof(c))];
int e[_Lengthof(typeof(d))];
int f[alignof(long)];

int g[(unsigned)123];
int h[(unsigned char)0x102];
int i[(unsigned long)(unsigned char)0xff];
int j[(long)42];
int k[(bool)42];

int l[-0xffff'fff0];
int m[~-4];
int n[!false];
int o[!!42u];
