#ifndef __PANKO_STDDEF_H
#define __PANKO_STDDEF_H

#define __STDC_VERSION_STDDEF_H__ 202311L

typedef typeof((char*)nullptr - (char*)nullptr) ptrdiff_t;
typedef typeof(sizeof(char)) size_t;

// TODO: `long double`
typedef long long max_align_t;

typedef int wchar_t;
typedef typeof(nullptr) nullptr_t;

#define NULL nullptr
#define unreachable() ((void)*(char*)nullptr)
// TODO: #define offsetof(__type, __member_designator)

#endif
