#include "sum.h"

#include <string.h>

double get_sum(double a, double b)
{
  return a + b;
}

unsigned int get_sum_ui(unsigned int a, unsigned int b)
{
  return a + b;
}

int get_num(const char* str)
{
  if (strcmp(str, "one") == 0) {
    return 1;
  } else if (strcmp(str, "two") == 0) {
    return 2;
  }
  return 0;
}

int arr_sum(int lst[5])
{
  int res = 0;
  for (int i = 0; i < 5; i++) {
    res += lst[i];
  }
  return res;
}

void arr_double(int in[5], int k, int out[5])
{
  for (int i = 0; i < 5; i++) {
    out[i] = k*in[i];
  }
}

int get_ij (int src[2][2], int i, int j)
{
  if (0 <= i && i <= 2 && 0 <= j && j <= 2) {
    return src[i][j];
  }
  return -1;
}

int dyn_sum(int * src, int len)
{
  int res = 0;
  for(int i = 0; i < len; i++) {
    res += src[i];
  }
  return res;
}

int dyn_sum2 (int ** src, int l1, int l2)
{
  int res = 0;
  for (int i = 0; i < l1; i++) {
    for (int j = 0; j < l2; j++) {
      res += src[i][j];
    }
  }
  return res;
}

void arr_triple(int v[5])
{
  for(int i = 0; i < 5; i++) {
    v[i] *= 3;
  }
}

float s_sum(struct AA x)
{
  return x.a + x.b;
}

float sp_sum(struct AA * x)
{
  return x->a + x->b;
}
