module Parser (
   parseSentinelFile,
   Target(Target)
) where

import Control.Monad
import qualified Control.Exception as E
import Text.ParserCombinators.Parsec

-- | A target is just a file-glob/command pair that a sentinel file is filled with
data Target = Target String String
   deriving Show

-- | Parses the end of a line
p_eol =   try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"

-- | Parses the end of a line or end of a file
p_end = p_eol <|> (eof >> return "")

-- | Parses a single target line, expecting the file-glob to the left-hand side of the
--   colon, and the command to the right. Can be terminated by line or by file
p_target :: Parser Target
p_target = do
   try p_eol
   g <- manyTill anyChar (try (char ':'))
   c <- manyTill anyChar (try p_end)
   return $ Target g c

-- | The target file parser is just a many lines of target instructions
p_targetFile = many p_target

-- | Takes a sentinel file's content and turns it into a set of targets
parseSentinelFile :: String -> Either ParseError [Target]
parseSentinelFile input = parse p_targetFile "(unknown)" input

