
module Codec.FFMpeg (
    initFFMpeg
    ) where

import Control.Eff

initFFMpeg :: IO ()
initFFMpeg = c_av_register_all

foreign import ccall "av_register_all"
    c_av_register_all :: IO ()
