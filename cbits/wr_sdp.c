#include <bluetooth/sdp.h>
#include "wr_sdp.h"

uuid_t *wr_sdp_profile_desc_get_uuid(const sdp_profile_desc_t *profile) {
    return &profile->uuid;
}