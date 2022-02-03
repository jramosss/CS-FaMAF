module Hal.Verification.THoare where

-- | Imports de Hal
import Hal.Lang


data GComm = Guard BExp
           | NGuard Comm
    deriving Show

type GuardComm = [GComm]
           

-- | Una terna de Hoare consiste de una precondición, un comando y una postcondición.
--   El comando debe ser lineal, es decir, no contener if, ni while, ni assert.
data THoare = THoare { pre :: FormFun
                     , comm :: GuardComm
                     , post :: FormFun
              }

instance Show THoare where
    show th = "[ { "++ (show $ pre th) ++ " },\n"++
                (show $ comm th) ++ ",\n"++
                "{ "++ (show $ post th) ++ " } ]\n"
