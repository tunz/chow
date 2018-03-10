#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case13(MyStruct *ptr) {
  if (ptr->elem1 == 1) { // XXX: null-pointer-dereference
    printf("one\n");
  }

  if (!ptr)
    printf("oops\n");
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case13(NULL);
}
