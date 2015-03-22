
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Codec.FFMpeg.Codec (
    decodeVideoFrames
    ) where

import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Foreign ( Ptr )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Storable ( peek )

import Codec.FFMpeg.Frame
import Codec.FFMpeg.Internal.Codec
import Codec.FFMpeg.Internal.Frame

decodeVideoFrames
    :: ( SetMember Lift (Lift IO) r
       , Member (Reader AVCodecContext) r
       , Member (Yield AVFrame) r
       , Member (Exc IOError) r
       )
    => Eff (Yield AVPacket :> r) ()
    -> Eff r ()
decodeVideoFrames pkt = ask >>= \ctx -> do
    f <- newAvFrame
    runC pkt >>= loop ctx f where
        loop :: ( SetMember Lift (Lift IO) r
                , Member (Yield AVFrame) r
                , Member (Exc IOError) r
                ) => AVCodecContext -> AVFrame -> Y r AVPacket () -> Eff r ()
        loop _ _ (Done ()) = return ()
        loop ctx f (Y p k)  = do
            (r, gp) <- lift $ alloca $ \gpp -> do
                r <- withAvPacket p $ \p' ->
                     withAvFrame f $ \f' ->
                     c_avcodec_decode_video2 ctx f' gpp p'

                peek gpp >>= \gp -> return (r, gp)

            if r < 0
                then throwExc $ userError $ "error decoding frame: " ++ show r
                else if gp == 0
                        then yield f >> k () >>= loop ctx f
                        else k () >>= loop ctx f

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

foreign import ccall "avcodec_decode_video2"
  c_avcodec_decode_video2 :: AVCodecContext -> Ptr AVFrame' -> Ptr CInt -> Ptr AVPacket' -> IO CInt
