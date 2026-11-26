int main() {
    typeof(int[5]) const xs = {1, 2, 3, 4, 5};
    // [[compile-error: implicit conversion from `ptr<int const>` to `ptr<int>` drops qualifiers]]
    int* p = xs;

    typedef long Array[10];
    Array const ys = {};
    // [[compile-error: implicit conversion from `ptr<long const>` to `ptr<long>` drops qualifiers]]
    long* p2 = ys;
}
