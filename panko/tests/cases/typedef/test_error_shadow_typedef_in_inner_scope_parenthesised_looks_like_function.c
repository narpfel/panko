int main() {
    typedef int name;
    {
        // these test the early-exit conditions of the long if-let chain in
        // `DirectDeclarator::reinterpret_as_concrete`
        int (name, ...);
        int (name, name);
        int (name const);
        int (const);
        int (int);
    }
}
