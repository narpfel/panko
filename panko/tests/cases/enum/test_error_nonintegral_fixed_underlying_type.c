enum A: void { A };
enum B: typeof(int*) { B };
enum Okay: long { Okay };
enum C: typeof(enum Okay) { C };
enum D: typeof(struct Incomplete) { D };
typedef void* Typedef;
enum E: Typedef { E };
