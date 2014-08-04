#include <alloca.h>
#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "wr_bluetooth.h"
#include "wr_sdp_lib.h"

int wr_sdp_set_browse_groups(sdp_record_t* rec, sdp_list_t* seq) {
    return sdp_set_browse_groups(rec, seq);
}

sdp_session_t *wr_sdp_connect(const wr_bdaddr_t *src, const wr_bdaddr_t *dst, const uint32_t flags) {
    bdaddr_t *newSrc = alloca(sizeof(bdaddr_t));
    bdaddr_t *newDst = alloca(sizeof(bdaddr_t));
    from_wr_bdaddr(newSrc, src);
    from_wr_bdaddr(newDst, dst);
    
    return sdp_connect(newSrc, newDst, flags);
}

int wr_sdp_set_service_classes(sdp_record_t* rec, sdp_list_t* seq) {
    return sdp_set_service_classes(rec, seq);
}

void wr_sdp_set_service_id(sdp_record_t *rec, const uuid_t *uuid) {
    sdp_set_service_id(rec, *uuid);
}