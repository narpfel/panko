typeof(int) main() {
    typeof(int const*) x = nullptr;
    typeof(typeof(int const)[5]) xs = {};
    x = xs;
    typeof(int const[5]) ys = {};
    (typeof(int)*)nullptr;
    return x[0] + ys[_Lengthof ys - 1];
}
