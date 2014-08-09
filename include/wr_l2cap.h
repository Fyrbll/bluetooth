#ifndef WR_L2CAP_H
#define WR_L2CAP_H

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include "wr_bluetooth.h"

typedef struct sockaddr_l2 sockaddr_l2_t;

wr_bdaddr_t *wr_sockaddr_l2_get_bdaddr(wr_bdaddr_t *dst, const sockaddr_l2_t *const src);
void wr_sockaddr_l2_set_bdaddr(sockaddr_l2_t *sa, const wr_bdaddr_t *const ba);

#endif