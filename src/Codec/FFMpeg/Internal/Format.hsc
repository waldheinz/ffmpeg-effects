
{-# LANGUAGE
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving
    #-}

module Codec.FFMpeg.Internal.Format (
    AVFormatContext(..), c_avformat_open_input, c_avformat_close_input
    ) where

import Data.Typeable ( Typeable )
import Foreign.C.String ( CString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable )

newtype AVFormatContext = AVFormatContext (Ptr ())
    deriving ( Storable, Typeable )

foreign import ccall "avformat_open_input"
  c_avformat_open_input :: Ptr AVFormatContext -> CString -> Ptr ()
                      -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "avformat_close_input"
  c_avformat_close_input :: Ptr AVFormatContext -> IO ()
