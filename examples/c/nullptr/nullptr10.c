#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case10(MyStruct *ptr) {
  printf("%d\n", ptr ? 1 : 2);
  printf("%d\n", ptr->elem1); // XXX: null-pointer-dereference
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case10(NULL);
}
