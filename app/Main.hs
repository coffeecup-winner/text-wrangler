module Main where

import Text.Wrangler

main :: IO ()
main = wrangle $ do
    every line from (every "*.hs" from currentFolder)
    output
