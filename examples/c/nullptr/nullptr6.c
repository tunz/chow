#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

// Not yet
void case6(MyStruct *ptr) {
  MyStruct *ptrs[10];
  ptrs[1] = ptr;
  printf("%d\n", (*(ptrs + 1))->elem1); // XXX: null-pointer-dereference

  if (ptrs[1] != NULL) {
    printf("%d\n", ptrs[1]->elem2);
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case6(NULL);
}
