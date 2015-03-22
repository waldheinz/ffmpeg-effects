
{-# LANGUAGE
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving
    #-}

module Codec.FFMpeg.Internal.Codec (

    AVCodecContext(..),

    AVCodec', AVCodec(..),

    AVPacket',

    AVPacket(..), newAVPacket, withAvPacket, avPacketStreamIndex
    ) where

#include <libavcodec/avcodec.h>

import Data.Typeable ( Typeable )
import Foreign ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr )
import Foreign.C.Types ( CInt(..) )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peekByteOff )

-------------------------------------------------------------------------------
-- AVCodecContext
-------------------------------------------------------------------------------

newtype AVCodecContext = AVCodecContext (Ptr ())
    deriving ( Typeable )

-------------------------------------------------------------------------------
-- AVCodec
-------------------------------------------------------------------------------

data AVCodec'

newtype AVCodec = AVCodec (Ptr AVCodec')
    deriving ( Typeable )

-------------------------------------------------------------------------------
-- AVPacket
-------------------------------------------------------------------------------

data AVPacket'

newtype AVPacket = AVPacket (ForeignPtr AVPacket')
    deriving ( Typeable )

newAVPacket :: IO AVPacket
newAVPacket = mallocForeignPtrBytes avPacketSize >>= return . AVPacket

withAvPacket :: AVPacket -> (Ptr AVPacket' -> IO a) -> IO a
withAvPacket (AVPacket fptr) = withForeignPtr fptr

avPacketStreamIndex :: AVPacket -> IO CInt
avPacketStreamIndex (AVPacket pp) = withForeignPtr pp (#peek AVPacket, stream_index)

avPacketSize :: Int
avPacketSize = (#size AVPacket)
