#include <stdio.h>

int main(void)
{
  int x, y;
  int f;

  x = 10;
  f = 1;

  while (x > 0) {
    f = f * x;
    x = x - 1;
  }

  y = f;

  printf ("%d\n", y);

  return 0;
}
