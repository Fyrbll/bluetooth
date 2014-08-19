#ifndef WR_RFCOMM_H
#define WR_RFCOMM_H

#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include "wr_bluetooth.h"

typedef struct sockaddr_rc sockaddr_rc_t;

wr_bdaddr_t *wr_sockaddr_rc_get_bdaddr(wr_bdaddr_t *dst, const sockaddr_rc_t *const src);
void wr_sockaddr_rc_set_bdaddr(sockaddr_rc_t *sa, const wr_bdaddr_t *const ba);

#endif