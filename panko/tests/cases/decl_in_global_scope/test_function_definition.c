int a() {
    return 42;
}

int b(int) {
    return 42;
}

int c(int a) {
    return a;
}

int d(int a) {
    {
        return a;
    }
}

int e() {
    {}
    ;
    return 42;
}

int f() {
    int a = 42;
    a;
    return a;
}

char g() {
    char c = 0;
    return c;
}

int h() {
    int a = 42;
    {
        int a = 27;
        return a;
    }
}

int i(int a) {
    {
        int a = 42;
    }
    return a;
}

int j() {
    int a = 42;
    {
        int a = 27;
        a;
    }
    return a;
}
