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
