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
void case3() {
  int i = 0, alloced = 0, val = *((int *)global); // XXX: null-pointer-dereference
  for (i = 0; i < 10; i++) {
    if (i == 1) {
      printf("%d\n", val);
    } else {
      if (global == NULL) {
        global = malloc(sizeof(MyStruct));
        alloced = 1;
      }
    }
  }
  if (alloced)
    free(global);
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  global = NULL;
  case3();
}
