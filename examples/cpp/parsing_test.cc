#include <iostream>

void loop_test() {
   char err_string[] =
     "\\"
     "'\"";

  int i = 1 == 1 ? 0 : -1;
  for (i = 0; i < 2; i++) {
    printf("Hi1\n\"");
  }
  for (i = 0; i < 3; i++)
    printf("%s\n", err_string);
  i = 0;
  while(i++ < 1)
    printf("Hi3\n");

  while(i < 3) {
    printf("Hi4\n");
    i += 1;
  }

  while(i < 10) {
    int j;
    for (j = 0; j < 2; j++)
      printf("Hi5\n");
    for (j = 0; j < 2; j++) {
      if (j == 1)
        continue;
      printf("Hi5\n");
      j += 1;
    }
    i += 1;
    if (i == 8)
      break;
  }

  i = 0;
  do {
    printf ("%d\n", i);
  } while (i < 10);
}

void if_test(int cond, int cond2) {
  int arr[10][20], *ptr, **ptr2;
  if (((cond == 1) && (cond2 != 2)) || (cond < 1 && cond2 >= 3)) {
    printf("branch 1 %d %d %d %p\n", arr[0][0], *ptr, **ptr2, &cond);
  } else if ((!!cond == 1) || (~~cond2 == 3) || cond2 & 1) {
    printf("branch 2\n");
  } else if (((cond ^ cond2) == 3) || (cond2-- - -cond == 123))
    printf("branch 3\n");
  else
    printf("branch 4 newline \n");

  switch (*ptr) {
    case 1:
      printf("1\n");
    case 2:
      printf("2\n");
      break;
    default:
      printf("default\n");
  }
}

int func1(int c) {
  loop_test();
  if_test(c, c+1);
  return c + 1;
}

int main() {
  int a = 123 + 1;
  func1(a);
}
