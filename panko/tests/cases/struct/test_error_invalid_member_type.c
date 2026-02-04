struct IncompleteMember {
    int x;
    void void_member;
    int y;
};

struct FunctionMember {
    int x;
    int function();
    int y;
};

struct Recursive {
    struct Recursive recursive;
};
