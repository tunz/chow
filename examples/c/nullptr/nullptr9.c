#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case9(int val, MyStruct *ptr1, MyStruct *ptr2) {
  printf("%d\n", ptr1->elem1); // False positive case
  if (val == 10)
    ptr1 = NULL;
  if (!ptr1)
    printf("nono\n");

  if (val > 1) {
    MyStruct *ptr2 = malloc(sizeof(MyStruct));
    printf("%d\n", ptr2->elem1); // False positive case
    if (val < 5) {
      MyStruct *ptr2 = malloc(sizeof(MyStruct));
      printf("%d\n", ptr2->elem1); // False positive case
    }
  }
  if (!ptr2)
    printf("nono\n");
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case9(val, NULL, NULL);
}
