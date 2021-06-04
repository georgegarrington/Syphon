--Functions used for testing the parsing
module Parse.Testing.Root where

import Control.Monad.Reader

import Text.Megaparsec

import Test.Hspec.Megaparsec

import Parse.Util

env = ParseEnv 0 False

testParse :: String -> Parser a -> Either (ParseErrorBundle String ParseErr) a 
testParse source p = runReader (runParserT (greedySc *> p <* eof) "" source) env