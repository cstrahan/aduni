/*
 * helper.h - Utility function declarations
 */

/* Protect against multiple inclusion */
#ifndef HELPER_H_
#define HELPER_H_

/*
 * Print an error message and exit
 * msg - message to print
 */
void err_quit(char *msg);

/*
 * Transfer data between two file descriptors
 * srcfd - input file descriptor
 * tgtfd - output file desriptor
 */
void xfer_data(int srcfd, int tgtfd);

/*
 * Interface to herror
 * Print an error message and exit
 * msg - message to print
 */
void herr_quit(char *msg);

#endif /* HELPER_H_ */
