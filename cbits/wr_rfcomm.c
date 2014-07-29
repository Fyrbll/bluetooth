#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>
#include "wr_rfcomm.h"

void wr_sockaddr_rc_set_rc_bdaddr(sockaddr_rc_t *sa, bdaddr_t *ba) {
    sa->rc_bdaddr = *ba;
}