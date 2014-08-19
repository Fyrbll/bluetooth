#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <string.h>
#include "wr_bluetooth.h"
#include "wr_hci.h"

wr_bdaddr_t *wr_hci_dev_info_get_bdaddr(wr_bdaddr_t *dst, const hci_dev_info_t *const src) {
    return to_wr_bdaddr(dst, &src->bdaddr);
}

void wr_hci_dev_info_set_bdaddr(hci_dev_info_t *di, const wr_bdaddr_t *const ba) {
    from_wr_bdaddr(&di->bdaddr, ba);
}

char *wr_hci_dev_info_get_name(char *dst, const hci_dev_info_t *const src) {
    return strcpy(dst, src->name);
}

void wr_hci_dev_info_set_name(hci_dev_info_t *di, const char *const name) {
    memset(di->name, 0, 8);
    strcpy(di->name, name);
}