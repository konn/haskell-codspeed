module Main (main) where

import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "haskell-codspeed" =<< getArgs
