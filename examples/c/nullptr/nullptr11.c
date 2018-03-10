#include <stdio.h>
#include <stdlib.h>

struct some_struct {
  int elem1;
  int elem2;
  int *ptrelem;
  struct some_struct *next;
};
typedef struct some_struct MyStruct;

MyStruct * make_new_struct(MyStruct *ptr) {
  if (ptr)
    free(ptr);

  return malloc(sizeof(MyStruct));
}

void case11(MyStruct *ptr1, MyStruct *ptr2, MyStruct *ptr3) {
  int val;

  if (ptr1 == NULL || ptr1->ptrelem == NULL)
    goto out;

  printf("%d\n", ptr1->elem1); // False positive case

  switch (ptr1->elem1) {
    case 1:
      if (!ptr3)
        break;
      printf("%d\n", ptr3->elem1);
      ptr3 = malloc(sizeof(MyStruct));
      if (!ptr3) // False positive case
        goto out;
      break;
    default:
      if (!ptr2->elem1 && !ptr2->elem2)
        break;
      if (!ptr2->elem1)
        printf("%d\n", ptr2->elem2);
  }
  if (ptr3)
    printf("test ahah\n");

  ptr1 = make_new_struct(ptr1);
  if (ptr1 == NULL) {
    ptr1 = make_new_struct(ptr1);
    if (ptr1 == NULL)
      return;
    val = 2;
  } else
    val = 1;

  printf("%d %d\n", val, ptr1->elem1); // False positive case

  ptr1 = make_new_struct(ptr1);
  if (ptr1)
    ptr1->elem1 = 0;
  if (!ptr1)
    return;
  ptr1->elem1 = 2; // Fales positive case
out:
  if (ptr1)
    printf("something");
  return;
}

int main() {
  int val = 10;
  MyStruct* ptr = malloc(sizeof(MyStruct));
  ptr->elem1 = 1;
  ptr->elem2 = 1;
  ptr->ptrelem = &val;

  case11(NULL, NULL, NULL);
}
