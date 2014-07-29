#ifndef WR_L2CAP_H
#define WR_L2CAP_H

#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>

typedef struct sockaddr_l2 sockaddr_l2_t;

void wr_sockaddr_l2_set_l2_bdaddr(sockaddr_l2_t *sa, bdaddr_t *ba);

#endif