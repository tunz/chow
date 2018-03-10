#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case14(MyStruct *ptr, int val) {
  if (ptr) {
    ptr->elem1 = 2; // False positive case
  }

  if (!ptr) {
    printf("??\n");
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case14(NULL, 2);
}
