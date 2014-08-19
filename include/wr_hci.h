#ifndef WR_HCI_H
#define WR_HCI_H

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include "wr_bluetooth.h"

typedef struct hci_dev_info hci_dev_info_t;

wr_bdaddr_t *wr_hci_dev_info_get_bdaddr(wr_bdaddr_t *dst, const hci_dev_info_t *const src);
void wr_hci_dev_info_set_bdaddr(hci_dev_info_t *di, const wr_bdaddr_t *const ba);
char *wr_hci_dev_info_get_name(char *dst, const hci_dev_info_t *const src);
void wr_hci_dev_info_set_name(hci_dev_info_t *di, const char *const name);

int wr_hci_get_dev_info();

#endif