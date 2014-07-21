#ifndef WR_SDP_H
#define WR_SDP_H

#include <bluetooth/sdp.h>

uuid_t *wr_sdp_profile_desc_get_uuid(const sdp_profile_desc_t *profile);

#endif