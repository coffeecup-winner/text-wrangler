module Main where

import Text.Wrangler

main :: IO ()
main = wrangle $ do
    every line from (every "Main.hs" from currentFolder)
    add "~" (around "(")
    add "++" (before "\"")
    add "--" (after "add")
    output
