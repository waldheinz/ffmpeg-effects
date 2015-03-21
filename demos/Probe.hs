
module Main ( main ) where

import Control.Eff.Exception
import Control.Eff.Lift
import System.Environment ( getArgs )

import Codec.FFMpeg
import Codec.FFMpeg.Format

probe :: String -> IO ()
probe fn = do
    putStrLn fn

    me <- runLift $ runExc (openInput fn (return (1 :: Int)))

    case me of
        Left e -> print (e :: IOError)
        Right _ -> return ()


    return ()

main :: IO ()
main = initFFMpeg >> getArgs >>= mapM_ probe
