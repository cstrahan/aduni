/*
 *  mknet.c
 */

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "helper.h"

int main(void)
{
  int sockfd;
  struct sockaddr_in srv;
  socklen_t socklen;
  int i = 1;

  /* Create the socket */
  if((sockfd = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    err_quit("socket");
  /* Want to reuse the local address */
  setsockopt(sockfd, SOL_SOCKET, 0, &i, sizeof(i));

  /* Initialize and set up the server structure */
  memset(&srv, 0, sizeof(srv));
  srv.sin_family = AF_INET;
  srv.sin_port = htons(50000); /*Don't forget network byte order! */

  /* Bing the socket to an address */
  socklen = sizeof(srv);
  if((bind(sockfd, (struct sockaddr *)&srv, socklen)) < 0)
    err_quit("bind");

  /* Wait for incoming connections */
  if((listen(sockfd, 5)) < 0)
    err_quit("listen");
  puts("TCP/IP socket available");
  printf("\tport %d\n", ntohs(srv.sin_port));
  printf("\taddr %s\n", inet_ntoa(srv.sin_addr));
  
  /* Loop forever, accepting all connections */
  while((accept(sockfd, (struct sockaddr *)&srv, &socklen)) >= 0)
    puts("new connection granted");

  exit(EXIT_SUCCESS);
}
