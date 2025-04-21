typedef int array[];

int main() {
    _Lengthof(int[]);
    // TODO: this should explicitly show that `array` is a `typedef`’d alias of
    // `int[]`
    // TODO: this has the `typedef`’s location as its primary location, which
    // means it will be sorted before the earlier error
    _Lengthof(array);
}
