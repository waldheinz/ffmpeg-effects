
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.FFMpeg.Internal.Util (
    AVMediaType(..), unknown, video, audio, subtitle, attachment
    ) where

#include <libavutil/avutil.h>

import Foreign.C ( CInt )
import Foreign ( Storable )

newtype AVMediaType = AVMediaType CInt deriving (Eq, Storable)

#{enum AVMediaType, AVMediaType
  , unknown     = AVMEDIA_TYPE_UNKNOWN
  , video       = AVMEDIA_TYPE_VIDEO
  , audio       = AVMEDIA_TYPE_AUDIO
  , subtitle    = AVMEDIA_TYPE_SUBTITLE
  , attachment  = AVMEDIA_TYPE_ATTACHMENT
  }
