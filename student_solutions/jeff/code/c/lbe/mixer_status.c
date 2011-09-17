/*
 * mixer_status.c - example program to display mixer settings.
 * Copyright (c) 1994-96 Jeff Tranter (jeff_tranter@mitel.com)
 * Heavily modified by Kurt Wall (kwall@xmission.com)
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <sys/soundcard.h>

/* Utility function for printing status */
void prn_stat(int condition);

int main(void)
{
    int fd;			/* File descriptor for mixer device */
    int level;			/* Volume setting */
    char *device = "/dev/mixer";
    /* Bitmasks for device settings */
    int recsrc, devmask, recmask, stereodevs, caps;
    /* Names of mixer channels */
    const char *labels[] = SOUND_DEVICE_LABELS;
    int i;

    /* Open the mixer device read only */
    if((fd = open(device, O_RDONLY)) < 0) {
	perror("open");
	exit(EXIT_FAILURE);
    }

    /* Get mixer information */
    if((ioctl(fd, SOUND_MIXER_READ_RECSRC, &recsrc)) < 0)
	perror("SOUND_MIXER_READ_RECSRC");
    if((ioctl(fd, SOUND_MIXER_READ_DEVMASK, &devmask)) < 0)
	perror("SOUND_MIXER_READ_DEVMASK");
    if((ioctl(fd, SOUND_MIXER_READ_RECMASK, &recmask)) < 0)
	perror("SOUND_MIXER_READ_RECMASK");
    if((ioctl(fd, SOUND_MIXER_READ_STEREODEVS, &stereodevs)) < 0)
	perror("SOUND_MIXER_READ_STEREODEVS");
    if((ioctl(fd, SOUND_MIXER_READ_CAPS, &caps)) < 0)
	perror("SOUND_MIXER_READ_CAPS");

    /* Print mixer information */
    printf("Status of %s:\n\n", device);
    printf("Mixer      Recording   Active     Stereo     Current\n");
    printf("Channel    Source      Source     Device      Level\n");
    printf("----------------------------------------------------\n");
     
    /* Loop over all devices */
    for (i = 0 ; i < SOUND_MIXER_NRDEVICES ; ++i) {
	/* Only interested in available devices */
	if((1 << i) & devmask) {
	    /* Print channel number and name */
	    printf("%2d %-8s", i, labels[i]);
	    /* Is it a recording source? */
	    prn_stat((1 << i) & recmask);
	    /* Is it active? */
	    prn_stat((1 << i) & recsrc);
	    /* Does it have stereo capability? */
	    prn_stat((1 << i) & stereodevs);
	    /* If stereo, show both levels */
	    if ((1 << i) & stereodevs) {
		if((ioctl(fd, MIXER_READ(i), &level)) < 0)
		    perror("SOUND_MIXER_READ");
		printf("  %3d%% %3d%%", level & 0xff, 
		       (level & 0xff00) >> 8);
	    } else { /* Only one channel */
		if((ioctl(fd, MIXER_READ(i), &level)) < 0)
		    perror("SOUND_MIXER_READ");
		printf("    %3d%%", level & 0xff);
	    }
	    printf("\n");
	}
    }
    /* Are recording sources exclusive? */
    printf("\nNote: Choices for recording source are ");
    if (!(caps & SOUND_CAP_EXCL_INPUT))
	printf("not ");
    printf("exclusive.\n");

    /* Close mixer device */
    close(fd);
    return 0;
}

void prn_stat(int condition)
{
    condition ? printf("   YES    ") : printf("   NO     ");
}
