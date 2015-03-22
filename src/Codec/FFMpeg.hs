
module Codec.FFMpeg (
    initFFMpeg,

    AVMediaType, video
    ) where

import Codec.FFMpeg.Internal.Util

initFFMpeg :: IO ()
initFFMpeg = c_av_register_all

foreign import ccall "av_register_all"
    c_av_register_all :: IO ()
