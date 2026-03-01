struct Incomplete;

struct Complete {
    int x;
};

struct Other {
    int x;
};

struct Incomplete* returns_incomplete();

int main() {
    struct Complete complete = {};
    struct Other other = {};

    false ? *returns_incomplete() : 42;
    true ? 42 : *returns_incomplete();
    true ? complete : other;
    false ? 42 : complete;
}
