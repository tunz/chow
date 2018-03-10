#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

MyStruct *global;
void case2() {
  int val = global->elem2; // XXX: null-pointer-dereference

  if (global != NULL) {
    printf("eleme2: %d\n", val);
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  global = NULL;
  case2();
}
