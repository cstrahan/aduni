/* getname.c - Get login names */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>

int main(void)
{
  char *login;
  struct passwd *pentry;

  /* Get the login name */
  if (( login = getlogin()) == NULL ) { /* oops */
    perror("getlogin");
    exit(EXIT_FAILURE);
  }
  printf("getlogin returned %s\n", login);
  
  /* get the password entry for login */
  if ((pentry = getpwnam(login)) == NULL) 
  {
    perror("getpwnam");
    exit(EXIT_FAILURE);
  }

  /* display the full name */
  printf("gecos: %s\n", pentry->pw_gecos);
  
  exit(EXIT_SUCCESS);
}

