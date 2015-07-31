#include <stdlib.h>
#include <stdio.h>

#define MAX(a,b) (a <= b ? b : a)

#define word_sign(w) (w & (1 << 30))

inline int word_field (int w, int l, int r)
{
  int result = w & ~(3 << 30);
  result = (result << ((MAX(l,1)-1)*6+2) >> ((MAX(l,1)-1)*6+2));
  result >>= (5-r)*6;
  return word_sign(result) ? -result : result;
}

#define word_of_int(n) \
  (n < 0 ? -n | (1 << 30) : n)

#define int_of_word(w) \
  (n & (1 << 30) ? -(n & ~(3 << 30)))

void print_word (int w)
{
  printf ("%c %02d %02d %02d %02d %02d", word_sign(w) ? '-' : '+',
      word_field(w,1,1), word_field(w,2,2), word_field(w,3,3),
      word_field(w,4,4), word_field(w,5,5));
}

typedef struct {
  int rJ;
  int rA;
  int rX;
  int rI[7];
  int overflow;
  int comparison;
  int memory[4000];
  int halted;
  int pc;
  int time;
} vm;

#define ABS(w) ((w) < 0 ? -(w) : (w))
#define FIELD5(w) (ABS(w) & 0x3f)
#define FIELD4(w) ((ABS(w) >> 6) & 0x3f)
#define FIELD3(w) ((ABS(w) >> 12) & 0x3f)
#define FIELD02(w) (((w) & (1<<30))?-((ABS(w) >> 18) & 0xfff):((ABS(w)>>18)&0xfff))

void step(vm * vm)
{
  int       inst = vm->memory[vm->pc];
  int          C = FIELD5(inst);
  int          F = FIELD4(inst);
  int          I = FIELD3(inst);
  int         AA = FIELD02(inst);
  int          M = AA + vm->rI[I];
  int * CONTENTS = vm->memory;

  vm->pc ++;

  switch (C) {
    case 0: /* NOP */
      vm->time += 1;
      break;
    case 8: /* LDA */
      vm->time += 2;
      vm->rA = field_word (CONTENTS[M], F / 8, F % 8);
      break;
    case 15: /* LDX */
      vm->time += 2;
      vm->rX = field_word (CONTENTS[M], F / 8, F % 8);
      break;
    case 48:
      switch (F) {
        case 2: /* ENTA */
          vm->time += 1;
          vm->rA = field_word (inst, 0, 2);
          break;
        default:
          fprintf (stderr, "instruction code %d, field code %d not implemented\n" C, F);
          exit (0);
          break;
      }
    default:
      fprintf(stderr,"instruction code %d not implemented\n", C);
      exit (0);
      break;
  }
}

int main (void)
{
  return 0;
}
