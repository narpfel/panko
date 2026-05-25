int a[42];
int b[sizeof 1];
int c[_Lengthof b];
int d[sizeof(typeof(c))];
int e[_Lengthof(typeof(d))];
int f[alignof(long)];
