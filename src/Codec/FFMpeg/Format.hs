
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Codec.FFMpeg.Format (
    openInput,

    seekFrame, readFrames
    ) where

import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Monad ( when )
import Data.Word ( Word64 )
import Foreign ( Ptr )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek, poke )

import Codec.FFMpeg.Internal.Codec
import Codec.FFMpeg.Internal.Format

openInput :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => String -> Eff (Reader AVFormatContext :> r) a -> Eff r a
openInput filename eff = do
    eavctx <- lift $ alloca $ \ctx ->
        withCString filename $ \cstr -> do
            poke (castPtr ctx) nullPtr
            r <- c_avformat_open_input ctx cstr nullPtr nullPtr
            if (r /= 0)
               then return $ Left $ "ffmpeg failed opening file: " ++ show r
               else peek ctx >>= return . Right

    case eavctx of
         Left e         -> throwExc $ userError e
         Right avctx    -> do
             r <- runReader eff avctx
             lift $ with avctx c_avformat_close_input
             return r

seekFrame
    :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r, Member (Reader AVFormatContext) r)
    => Integer
    -> Eff r ()
seekFrame ts = ask >>= \ctx -> lift (c_av_seek_frame ctx (-1) (fromIntegral ts) 0) >>=
    \r -> when (r < 0) $ throwExc  (userError $ "seek failed: " ++ show r)

readFrames
    :: (SetMember Lift (Lift IO) r, Member (Reader AVFormatContext) r, Member (Yield AVPacket) r)
    => Eff r ()
readFrames = ask >>= \ctx -> do
    p <- lift newAVPacket
    r <- lift $ withAvPacket p (c_av_read_frame ctx)
    if r == 0
       then yield p >> readFrames
       else return ()

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

foreign import ccall "av_seek_frame"
  c_av_seek_frame :: AVFormatContext -> CInt -> Word64 -> CInt -> IO CInt

foreign import ccall "av_read_frame"
  c_av_read_frame :: AVFormatContext -> Ptr AVPacket' -> IO CInt
