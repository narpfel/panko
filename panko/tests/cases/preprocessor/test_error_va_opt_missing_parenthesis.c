#define F1(x, ...) __VA_OPT__)
#define F2(x, ...) __VA_OPT__ 42
#define F3(x, ...) __VA_OPT__(42
#define F4(x, ...) __VA_OPT__(
#define F5(x, ...) __VA_OPT__
