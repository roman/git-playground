{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import HSH ((-|-))
import qualified HSH as Shell
import qualified Data.Attoparsec.Char8 as Parser
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

-------------------------------------------------------------------------------

newtype CommitFileInfo 
  = CFI { 
    unwrapCFI :: (Int, Int, ByteString) 
  } deriving (Show)

data Commit
  = Commit {
    commitId      :: ByteString
  , commitAuthor  :: ByteString
  , commitEmail   :: ByteString
  , commitSubject :: ByteString
  , commitFiles   :: [CommitFileInfo]
  }
  deriving (Show)

-------------------------------------------------------------------------------

gitLog :: IO ByteString
gitLog =
    BS.pack <$> Shell.run command
  where
    command = "git log --pretty=\"%h, %cn, %ce, %s\"\
 \--numstat --author=roman" :: String

countLines :: [Commit] -> Int
countLines = foldr helper 0
  where
    helper (Commit { commitFiles = fs }) acc = acc + (sum $ map helper2 fs)
    helper2 (CFI (a, _, _)) = a
     
-------------------------------------------------------------------------------

pToken = Parser.takeTill (\c -> c == ',')
       <* Parser.char ','
       <* Parser.skipSpace
pEOL = Parser.takeTill 
    (\c -> c == '\n' || c == '\r') <* Parser.skipSpace

pString = Parser.takeTill Parser.isSpace <* Parser.skipSpace
pNumber = Parser.decimal <* Parser.skipSpace
pCommitFileInfo = CFI <$> pTriple
pTriple = (,,) <$> pNumber 
               <*> pNumber
               <*> pString

pCommit =  Commit <$> pToken
                  <*> pToken
                  <*> pToken
                  <*> pEOL
                  <*> (Parser.many1 pCommitFileInfo)
-------------------------------------------------------------------------------

main :: IO ()
main = Shell.bracketCD "/home/vagrant/.vim" $
  gitLog >>= \e -> 
  case Parser.parseOnly (Parser.many pCommit) e of
    Right result -> print $ countLines result
    Left e -> print e

