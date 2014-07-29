#include <bluetooth/bluetooth.h>
#include "wr_bluetooth.h"

bdaddr_t *wr_bdaddr_any() {
    return BDADDR_ANY;
}

bdaddr_t *wr_bdaddr_local() {
    return BDADDR_LOCAL;
}

int wr_htobs(int n) {
    return htobs(n);
}