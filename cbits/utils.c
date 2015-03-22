
#include "utils.h"

void av_frame_free_helper(AVFrame *frame) {
    AVFrame **tmp = &(AVFrame *){ frame };
    av_frame_free(tmp);
}
