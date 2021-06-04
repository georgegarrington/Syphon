module Transpile.Util where

tabs :: Int -> String
tabs n = replicate n '\t'

writeDfnArgs :: Int -> String
writeDfnArgs n = concatMap (\x -> "$arg" ++ (show x) ++ " => ") [0..(n-1)]