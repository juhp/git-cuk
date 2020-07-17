module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)

import Cuk (cuk)

main :: IO ()
main = hSetEncoding stdout utf8 >> cuk
