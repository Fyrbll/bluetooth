#ifndef WR_SDP_LIB_H
#define WR_SDP_LIB_H

#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>

void wr_sdp_set_service_id(sdp_record_t *rec, const uuid_t *uuid);

#endif