
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Codec.FFMpeg.Format (
    openInput
    ) where

import Control.Eff
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Foreign.C.String ( withCString )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek, poke )

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
