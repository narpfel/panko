typedef char AsParameter[];

void f(AsParameter const parameter) {}

int main() {
    typeof(int[5]) const xs = {1, 2, 3, 4, 5};

    typedef long Array[10];
    Array const ys = {};
}
