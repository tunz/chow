#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case4(int *ptr) {
  int *ptrs[10];
  ptrs[0] = ptr;
  printf("%d\n", *ptrs[0]); // XXX: null-pointer-dereference

  if (ptrs[0] != NULL) {
    printf("%d\n", *ptrs[0]);
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case4(NULL);
}
