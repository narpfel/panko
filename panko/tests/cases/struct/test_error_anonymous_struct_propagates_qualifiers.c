struct PropagatesQualifiers {
    int x;
    struct {
        int y;
        long l;
    } const;
};

int main() {
    struct PropagatesQualifiers p = {};
    // okay
    p.x = 1;
    // not okay
    p.y = 2;
    p.l = 3;
}
