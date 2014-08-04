#ifndef WR_BLUETOOTH_H
#define WR_BLUETOOTH_H

#include <bluetooth/bluetooth.h>
#include <stdint.h>

typedef struct {
    uint8_t b1, b2, b3, b4, b5, b6;
} wr_bdaddr_t;

bdaddr_t *from_wr_bdaddr(bdaddr_t *dst, const wr_bdaddr_t *const src);
wr_bdaddr_t *to_wr_bdaddr(wr_bdaddr_t *dst, const bdaddr_t *const src);
wr_bdaddr_t *wr_bdaddr_alloc(const bdaddr_t *const ba);
const wr_bdaddr_t *const wr_bdaddr_any();
const wr_bdaddr_t *const wr_bdaddr_local();
int wr_htobs(int n);

#endif