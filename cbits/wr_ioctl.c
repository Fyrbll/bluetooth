#include <sys/ioctl.h>
#include "wr_ioctl.h"

int wr_ioctl(const int d, const int request, void *argp) {
    return ioctl(d, request, argp);
}