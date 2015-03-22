
{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Codec.FFMpeg.Frame (
    AVFrame, newAvFrame, withAvFrame
    ) where

import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Exception
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Control.Monad ( when )
import Data.Word ( Word64 )
import Foreign ( Ptr, ForeignPtr, newForeignPtr, withForeignPtr )
import Foreign.C.String ( withCString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( castPtr, nullPtr )
import Foreign.Storable ( peek, poke )

import Codec.FFMpeg.Internal.Frame

newAvFrame :: (SetMember Lift (Lift IO) r, Member (Exc IOError) r) => Eff r AVFrame
newAvFrame = lift c_av_frame_alloc >>=
    \ptr -> if ptr == nullPtr
               then throwExc (userError "frame alloc failed")
               else lift (newForeignPtr (c_av_frame_free_helper) (castPtr ptr)) >>= return . AVFrame

withAvFrame :: AVFrame -> (Ptr AVFrame' -> IO a) -> IO a
withAvFrame (AVFrame fptr) = withForeignPtr fptr

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

foreign import ccall "av_frame_alloc"
  c_av_frame_alloc :: IO (Ptr ())
