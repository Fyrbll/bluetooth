#include "wr_bluetooth.h"
#include "wr_rfcomm.h"

wr_bdaddr_t *wr_sockaddr_rc_get_bdaddr(wr_bdaddr_t *dst, const sockaddr_rc_t *const src) {
    return to_wr_bdaddr(dst, &src->rc_bdaddr);
}

void wr_sockaddr_rc_set_bdaddr(sockaddr_rc_t *sa, const wr_bdaddr_t *const ba) {
    from_wr_bdaddr(&sa->rc_bdaddr, ba);
}