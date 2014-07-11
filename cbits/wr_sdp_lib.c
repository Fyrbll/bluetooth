#include <bluetooth/sdp.h>
#include <bluetooth/sdp_lib.h>
#include "wr_sdp_lib.h"

void wr_sdp_set_service_id(sdp_record_t *rec, uuid_t *uuid) {
  sdp_set_service_id(rec, *uuid);
}