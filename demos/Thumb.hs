
{-# LANGUAGE ScopedTypeVariables #-}

module Main ( main ) where

import Control.Eff
import Control.Eff.Coroutine
import Control.Eff.Exception
import Control.Eff.Lift
import System.Environment ( getArgs )

import Codec.FFMpeg
import Codec.FFMpeg.Codec
import Codec.FFMpeg.Demux
import Codec.FFMpeg.Format

probe :: String -> IO ()
probe fn = do
    putStrLn fn

    me <- runLift $ runExc $ openInput fn $ do
        seekFrame 120000
        (si, avc) <- findBestStream video

        let
            loop (Done _) = lift $ putStrLn "done"
            loop (Y pkt k) = do
                psi <- packetStreamIndex pkt

                if psi == si
                   then lift (putStrLn ".")
                   else lift (putStrLn "-")

                k () >>= loop


        runC readFrames >>= loop


        return ()

    case me of
        Left e -> print (e :: IOError)
        Right _ -> return ()

    return ()

main :: IO ()
main = initFFMpeg >> getArgs >>= mapM_ probe
