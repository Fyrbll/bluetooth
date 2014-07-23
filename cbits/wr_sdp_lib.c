#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "wr_sdp_lib.h"

int wr_sdp_set_browse_groups(sdp_record_t* rec, sdp_list_t* seq) {
    return sdp_set_browse_groups(rec, seq);
}

int wr_sdp_set_service_classes(sdp_record_t* rec, sdp_list_t* seq) {
    return sdp_set_service_classes(rec, seq);
}

void wr_sdp_set_service_id(sdp_record_t *rec, const uuid_t *uuid) {
    sdp_set_service_id(rec, *uuid);
}