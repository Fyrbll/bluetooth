#ifndef WR_SDP_LIB_H
#define WR_SDP_LIB_H

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "wr_bluetooth.h"

int wr_sdp_set_browse_groups(sdp_record_t* rec, sdp_list_t* seq);
sdp_session_t *wr_sdp_connect(const wr_bdaddr_t *src, const wr_bdaddr_t *dst, const uint32_t flags);
int wr_sdp_set_service_classes(sdp_record_t* rec, sdp_list_t* seq);
void wr_sdp_set_service_id(sdp_record_t *rec, const uuid_t *uuid);

#endif