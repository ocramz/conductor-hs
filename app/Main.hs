module Main where

import Web
-- import Network.Wai

import Lib 

import Text.Printf (printf)

main :: IO ()
main = putStrLn "hi!"




-- scotty

-- mainWeb :: Parsable t => Int -> (t -> ActionM ()) -> IO ()
-- mainWeb port f = scotty port $ 
--   handleSubs f
