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

// TODO: this should error for both lhs and rhs
static int k = (0x7fff'ffff + 1) << 0x1'0000'0000;
static int l = (0x7fff'ffff + 1) << 100;

static unsigned m = 1u / 0u;
static unsigned n = 1u % 0u;

// TODO: this should error for both lhs and rhs
static int o = (0x7fff'ffff + 1) / 0;

static int not_constexpr = 42;
static int p = not_constexpr = 1 / 0;
