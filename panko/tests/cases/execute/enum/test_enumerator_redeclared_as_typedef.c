enum E { Type };

// [[compile-error: enumerator name `Type` redeclared as `typedef` name]]
typedef int Type;
