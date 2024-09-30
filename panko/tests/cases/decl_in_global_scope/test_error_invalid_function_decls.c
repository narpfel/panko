// can’t return function
int a()();

// okay
int (*b)();

// fn(∅: void) -> ptr<fn() -> fn() -> int>
int ((*c(void))())();

// can’t have named `void` param
int d(void param);

// okay
int e(void);
