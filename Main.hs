{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Control.Exception as E
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Either
import Data.Maybe
import Data.String.Utils
import System.INotify
import System.Directory
import System.FilePath.Glob
import System.Process
import Parser (parseSentinelFile, Target(Target))

-- | The file locations that we'll attempt to read upon invocation
locations = [ "./.sentinel", "~/.sentinel" ]

-- | Prepares a glob pattern from a target
makePattern (Target g _) = compile g
-- | Executes the shell command for a target
executeCommand (Target _ c) = runCommand c

-- | Flattens a 2d list (a list of lists) into a single list
flatten :: [[a]] -> [a]
flatten [] = []
flatten (g:gs) = g ++ (flatten gs)

-- | Extracts a string from a Maybe FilePath
exFN :: Maybe FilePath -> String
exFN path = head (maybeToList path)

-- | Reads each of the passed in files and returns either the files' content where there
--   was a successful read, otherwise Nothing
safeReadFile :: String -> IO (Maybe String)
safeReadFile path = do
   res <- E.try $ readFile path
   case res of
      Left (e :: E.IOException) -> return Nothing
      Right c -> return $ Just c

-- | Collects all of the successfully read contents of the read files into an array of
--   targets
allContents :: [String] -> IO [Target]
allContents paths = do
   rs <- safeReadFiles
   let cs = map parseSentinelFile (catMaybes rs)
   return $ flatten $ rights cs
 where safeReadFiles = mapM safeReadFile paths

-- | Filters a list of targets for a given event
filterTargets :: Event -> [Target] -> IO [Target]
filterTargets Modified { maybeFilePath = path } ts = do
   return $ filter (\t -> match (makePattern t) f) ts
 where f = exFN path

-- | Substitutes out magic variables for their real counterpart values
substituteCommands :: Event -> [Target] -> IO [Target]
substituteCommands e ts = do
   return $ map (scmd e) ts
 where scmd Modified { maybeFilePath = path } (Target g c) = Target g c'
        where f = exFN path
              c' = replace "{fn}" f c

-- | Performs the actions defined for targets associated to this event
handleFile :: Event -> IO ()
handleFile e = allContents locations
           >>= filterTargets e
           >>= substituteCommands e
           >>= mapM_ executeCommand

main :: IO ()
main = do
   workingDir <- getCurrentDirectory
   inotify <- initINotify
   _ <- addWatch inotify [Modify] workingDir handleFile
   forever $ do
      threadDelay 1000


