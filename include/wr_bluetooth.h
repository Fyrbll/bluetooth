#ifndef WR_BLUETOOTH_H
#define WR_BLUETOOTH_H

#include <bluetooth/bluetooth.h>

bdaddr_t *wr_bdaddr_any();
bdaddr_t *wr_bdaddr_local();

#endif