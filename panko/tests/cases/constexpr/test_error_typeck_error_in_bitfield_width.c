struct T {
    int x:sizeof(int[]);
    int y:_Lengthof(long[]);
};
