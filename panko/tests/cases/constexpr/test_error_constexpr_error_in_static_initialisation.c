static int a = -1 << 1;
static int b = 1 << -1;
static int c = 1 >> -1;
static int d = 1 << 32;
static int e = 1 << 31;
static int f = 1 << 0x1'0000'0000;

static int g = 1 / 0;
static int h = (-0x7fff'ffff - 1) / -1;

static int i = 1 % 0;
static int j = (-0x7fff'ffff - 1) % -1;

// this errors for both lhs and rhs
static int k = (0x7fff'ffff + 1) << 0x1'0000'0000;
static int l = (0x7fff'ffff + 1) << 100;

static unsigned m = 1u / 0u;
static unsigned n = 1u % 0u;

// this errors for both lhs and rhs
static int o = (0x7fff'ffff + 1) / 0;

static int not_constexpr = 42;
static int p = not_constexpr = 1 / 0;

static int q = (1 / 0, 0);
static int r = (1, 1 / 0);
static int s = (1 / 0, 1 / 0);

void logical() {
    static int a = 1 / 0 && 1 / 0;
    static int b = 1 && 1 / 0;

    static int c = 0 || 1 / 0;
    static int d = 1 / 0 || 1 / 0;
}

void conditional() {
    static int a = 1 ? 1 / 0 : 1 / 0;
    static int b = 0 ? 1 / 0 : 1 / 0;

    static int c = 1 / 0 ? 1 / 0 : 1 / 0;
}

void compound_literal() {
    struct T {
        int x;
        int y;
    };
    static struct T a = (struct T){.x = 1, .y = 1 / 0};
    static struct T b = (struct T){.x = 1, .y = 1 + 2};
}

static int t = ((void)(1 / 0), 42);

void pointers() {
    static int a;
    static int* b = &a;
    static int* c = &*b;
}

void error_in_bitshift_operands() {
    static int x = (1 / 0) << (1 / 0);
}

void comparison_between_pointers_into_different_objects() {
    static int x = 42;
    static int y = 27;
    static int comparison_result = &x < &y;
}

void error_in_static_initialiser_of_bitfield_member() {
    struct T {
        int bitfield:10;
    };
    static struct T value = {.bitfield = (1 / 0) * (1 / 0)};
}
