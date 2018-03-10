# Chow

Static Analysis Checker with Micro-Grammar based on this academic paper [How to build static checking systems using orders of magnitude less code](https://web.stanford.edu/~mlfbrown/paper.pdf). Current version only supports Null Pointer Dereference Checker targeting C code as a default.

## Example

Run chow on `examples/c/nullptr/nullptr1.c`.
```c
...
void case1(MyStruct *ptr) {
  printf("elem1: %d\n", ptr->elem1); // XXX: null-pointer-dereference

  if (!ptr) {
    // Use-then-check
    printf("elem2: %d\n", ptr->elem2);
  } else {
    // Check-then-use
    printf("eleme2 is null: %d\n", ptr->elem2);
  }
}
...
```

```
$ cabal build
$ ./dist/build/chow-cli/chow-cli ./examples/c/nullptr/nullptr1.c
Language: C
Checker: Default
Running Time: 0.002030201s
[Detect]
ptr is dereferenced in "./examples/c/nullptr/nullptr1.c" (line 13, column 25)
ptr is believed to be 'IsBoth' in "./examples/c/nullptr/nullptr1.c" (line 15, column 10)
```
