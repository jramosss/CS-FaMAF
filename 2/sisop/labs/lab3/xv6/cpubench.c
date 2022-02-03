#include "types.h"
#include "stat.h"
#include "user.h"
#include "fs.h"
#include "fcntl.h"

#define N 128
#define TIMES 32
#define MINTICKS 250

static float a[N][N];
static float b[N][N];
static float c[N][N];

static void init(void) {
  int x, y;
  for (y = 0; y < N; ++y) {
    for (x = 0; x < N; ++x) {
      a[y][x] = y - x;
      b[y][x] = x - y;
      c[y][x] = 0.0f;
    }
  }
}

static void matmul(float beta) {
  int x, y, k;
  for (y = 0; y < N; ++y) {
    for (x = 0; x < N; ++x) {
      for (k = 0; k < N; ++k) {
        c[y][x] += beta * a[y][k] * b[k][x];
      }
    }
  }
}

int
main(int argc, char *argv[])
{
  int pid = getpid();
  float beta = 1.0f;

  init();
  int start = uptime();
  long ops = 0;
  for(;;) {
    int end = uptime();
    int elapsed = end - start;
    if (elapsed >= MINTICKS) {
        printf(1, "%d: %d MFLOPT\n", pid, (int) ((ops / 1000) / elapsed));

        start = end;
        ops = 0;
    }

    for(int i = 0; i < TIMES; ++i) {
        matmul(beta);
        beta = -beta;
        ops += 3 * N * N * N;
    }
  }

  printf(1, "%x\n", c[0][0]);
  exit();
  return 0;
}

