// [[known-bug: should show “redeclared as enumerator name”]]

typedef int Name;

// [[compile-error: `typedef` name `Name` redeclared as enumerator name]]
enum { Name };
