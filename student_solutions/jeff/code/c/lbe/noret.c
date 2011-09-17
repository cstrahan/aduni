/*
 * noret.c
 */

void die_now(void) __attribute__ ((noreturn));

int main(void)
{
  int i = 0;

  while(1) {
    switch(i) {
    case 0 ... 5:
      printf("i = %d\n", i);
      break;
    default:
      die_now();
    }
    i++;
  }
  return 0;
}

void die_now(void)
{
  printf("dying now\n");
  exit(1);
}
