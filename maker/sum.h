/*{
  compiler="gcc",
  name="libsum",
  lib="-shared -fPIC",
  flags="-Wall"
}*/

struct AA {
  int a;
  float b;
};

/*{}*/
double get_sum(double a, double b);

/*{}*/
unsigned int get_sum_ui(unsigned int a, unsigned int b);

/*{}*/
int get_num(const char* str);

/*{name="arr5_sum"}*/
int arr_sum(int lst[5]);

/*{}*/
int get_ij (int src[2][2], int i, int j);

/*{}*/
int dyn_sum(int * src, int len);

/*{}*/
int dyn_sum2 (int ** src, int l1, int l2);

/*{out="out"}*/
void arr_double(int in[5], int k, int out[5]);

/*{inout="v"}*/
void arr_triple(int v[5]);

/*{}*/
float s_sum(struct AA x);

/*{ptr="x"}*/
float sp_sum(struct AA * x);
