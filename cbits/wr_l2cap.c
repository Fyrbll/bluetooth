#include <bluetooth/bluetooth.h>
#include <bluetooth/l2cap.h>
#include "wr_l2cap.h"

void wr_sockaddr_l2_set_l2_bdaddr(sockaddr_l2_t *sa, bdaddr_t *ba) {
    sa->l2_bdaddr = *ba;
}