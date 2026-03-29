struct Struct {
    int member;
    char another_member;
};

union Union {
    int union_member;
    char also_member;
};

int main() {
    struct Struct s = {.member = 42, .this_does_not_exist = 123};
    struct Struct s2 = {.member.submember = 0};
    union Union u = {.union_member = 42, 27};
    union Union u2 = {.union_member = 42, .also_does_not_exist = 123};
    union Union u3 = {.union_member.wrong.member = 123};
}
