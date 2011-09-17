/*
 * showit.c _ Display driver
 */

#include <stdio.h>
#include "msg.h"

int main(void)
{
  char msg_hi[] = {"Hi there, programmer!"};
  char msg_bye[] = {"Goodbye, programmer!"};
  printf("%s\n", msg_hi);
  prmsg(msg_bye);
  return 0;
}


  
