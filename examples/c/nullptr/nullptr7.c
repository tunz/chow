#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

void case7(MyStruct *ptr) {
  struct some_struct *p; // False positive case
  MyStruct *a, *p2 = NULL, *p3 = NULL; // False positive case
  a = NULL;
  if (a != 0)
    return;

  p = ptr;
  p = p->next; // False positive case
  printf("%d\n", p->elem1); // False positive case
  printf("%d\n", p2 && p2->elem1); // False positive case
  printf("%d\n", !p2 || p2->elem1); // False positive case
  printf("%d\n", (p3 || p2) && p2->elem1); // XXX: null-pointer-dereference
  p = malloc(p->elem1);
  if (!p) {
    printf("nono\n");
  }
  if (!p2) {
    printf("nono\n");
  }
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case7(NULL);
}
