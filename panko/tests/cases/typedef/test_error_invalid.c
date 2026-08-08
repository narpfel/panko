typedef int T1:20;
typedef int T2:undefined;
// does not error on the `sizeof` of incomplete type because that is checked later
typedef int T3:sizeof(void);
