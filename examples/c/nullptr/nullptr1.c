#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

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

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case1(ptr);
  case1(NULL);
}
