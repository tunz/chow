#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case8(int val, MyStruct *ptr) {
  printf("%d\n", ptr->elem1); // XXX: null-pointer-dereference

  switch (val) {
    case 1:
      break;
    case 2:
      if (ptr != NULL) {
        printf("%d\n", ptr->elem2);
      }
      break;
    default:
      break;
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case8(val, NULL);
}
