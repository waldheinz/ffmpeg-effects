
{-# LANGUAGE
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving
    #-}

module Codec.FFMpeg.Internal.Frame (
    AVFrame(..), c_av_frame_free_helper,

    AVFrame'
    ) where

#include <utils.h>

import Data.Typeable ( Typeable )
import Foreign ( ForeignPtr )
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr, FunPtr )
import Foreign.Storable ( Storable )

data AVFrame' = AVFrame'

newtype AVFrame = AVFrame (ForeignPtr AVFrame')
    deriving ( Typeable )

foreign import ccall "utils.h &av_frame_free_helper"
  c_av_frame_free_helper :: FunPtr (Ptr AVFrame' -> IO ())
