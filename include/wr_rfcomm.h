#ifndef WR_RFCOMM_H
#define WR_RFCOMM_H

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>

typedef struct sockaddr_rc sockaddr_rc_t;

void wr_sockaddr_rc_set_rc_bdaddr(sockaddr_rc_t *sa, bdaddr_t *ba);

#endif