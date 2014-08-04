#include <bluetooth/bluetooth.h>
#include <stdlib.h>
#include "wr_bluetooth.h"

wr_bdaddr_t *WR_BDADDR_ANY = NULL;
wr_bdaddr_t *WR_BDADDR_LOCAL = NULL;

bdaddr_t *from_wr_bdaddr(bdaddr_t *dst, const wr_bdaddr_t *const src) {
    dst->b[0] = src->b1;
    dst->b[1] = src->b2;
    dst->b[2] = src->b3;
    dst->b[3] = src->b4;
    dst->b[4] = src->b5;
    dst->b[5] = src->b6;
    return dst;
}

wr_bdaddr_t *to_wr_bdaddr(wr_bdaddr_t *dst, const bdaddr_t *const src) {
    dst->b1 = src->b[0];
    dst->b2 = src->b[1];
    dst->b3 = src->b[2];
    dst->b4 = src->b[3];
    dst->b5 = src->b[4];
    dst->b6 = src->b[5];
    return dst;
}

wr_bdaddr_t *wr_bdaddr_alloc(const bdaddr_t *const ba) {
    wr_bdaddr_t *wba = malloc(sizeof(wr_bdaddr_t));
    return to_wr_bdaddr(wba, ba);
}

const wr_bdaddr_t *const wr_bdaddr_any() {
    if (WR_BDADDR_ANY == NULL) {
        WR_BDADDR_ANY = wr_bdaddr_alloc(BDADDR_ANY);
    }
    return WR_BDADDR_ANY;
}

const wr_bdaddr_t *const wr_bdaddr_local() {
    if (WR_BDADDR_LOCAL == NULL) {
        WR_BDADDR_LOCAL = wr_bdaddr_alloc(BDADDR_LOCAL);
    }
    return WR_BDADDR_LOCAL;
}

int wr_htobs(int n) {
    return htobs(n);
}