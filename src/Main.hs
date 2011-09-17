module Main where

import qualified Github.Types as Github
import qualified Github.Request as Github

main :: IO ()
main = Github.getReposForUser "roman" >>= print
