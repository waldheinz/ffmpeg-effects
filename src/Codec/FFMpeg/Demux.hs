
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Codec.FFMpeg.Demux (
    StreamIndex, findBestStream, withStream,

    AVPacket, packetStreamIndex

    ) where

import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Monad ( join )
import Foreign ( Ptr )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable ( peek )

import Codec.FFMpeg.Internal.Codec
import Codec.FFMpeg.Internal.Format
import Codec.FFMpeg.Internal.Util

newtype StreamIndex = StreamIndex { _unStreamIndex :: CInt }
    deriving ( Eq, Show )

packetStreamIndex :: (SetMember Lift (Lift IO) r) => AVPacket -> Eff r StreamIndex
packetStreamIndex p = lift $ avPacketStreamIndex p >>= return . StreamIndex


withStream sn = loop where
    loop (Done ()) = return ()
    loop (Y p k) = do
        pidx <- packetStreamIndex p

        if pidx == sn
            then yield p >> k () >>= loop
            else k () >>= loop

findBestStream
    :: ( SetMember Lift (Lift IO) r
       , Member (Reader AVFormatContext) r
       , Member (Exc IOError) r
       )
    => AVMediaType
    -> Eff r (StreamIndex, AVCodec)
findBestStream tp = ask >>= \fmtCtx ->
    join $ lift $ alloca $ \decRet -> do
        sn <- c_av_find_best_stream fmtCtx tp (-1) (-1) decRet 0
        if sn >= 0
            then peek decRet >>= \dec -> return $ return (StreamIndex sn, AVCodec dec)
            else return $ throwExc $ userError $ "error determining best stream: " ++ show sn

-------------------------------------------------------------------------------
-- FFI imports
-------------------------------------------------------------------------------

foreign import ccall "av_find_best_stream"
    c_av_find_best_stream :: AVFormatContext -> AVMediaType -> CInt -> CInt
        -> Ptr (Ptr AVCodec') -> CInt -> IO CInt
