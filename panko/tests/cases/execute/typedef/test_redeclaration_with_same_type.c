// redeclarations of `typedef`s are allowed when declaring the same type

typedef long in_global_scope;
typedef long in_global_scope;

int main() {
    typedef int in_local_scope;
    typedef int in_local_scope;
}
