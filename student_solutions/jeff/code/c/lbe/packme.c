/*
 * packme.c
 */

#include <stdio.h>

/* a "packed" structure */
struct P {
  short s[3];
  long l;
  char str[5];
} __attribute__ ((packed));

/* a normal unpacked structure */
struct UP {
  short s[3];
  long l;
  char str[5];
};

int main(void)
{
  struct P packed;
  struct UP unpacked;

  /* print the size of each member for comparison */
  fprintf(stdout, "sizeof char str[5] = %d bytes\n", sizeof(char) * 5);
  fprintf(stdout, "      sizeof long  = %d bytes\n", sizeof(long));
  fprintf(stdout, " sizeof short s[5] = %d bytes\n", sizeof(short) * 3);

  /* how big are the two structures? */
  fprintf(stdout, "  sizeof packed = %d bytes\n", sizeof packed);
  fprintf(stdout, "sizeof unpacked = %d bytes\n", sizeof unpacked);

  return 0;
}
	  
