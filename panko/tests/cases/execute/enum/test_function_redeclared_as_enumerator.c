// [[known-bug: redeclaration error for function → enumerator does not show `function` in error message]]

int Function() {
    return 42;
}

// [[compile-error: function name `Function` redeclared as enumerator name]]
enum E { Function };
