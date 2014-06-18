{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Control.Exception as E
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Either
import Data.Maybe
import System.INotify
import System.Directory
import System.FilePath.Glob
import Parser (parseSentinelFile, makePattern, executeCommand, Target)

-- | The file locations that we'll attempt to read upon invocation
locations = [ "./.sentinel", "~/.sentinel" ]

-- | Reads each of the passed in files and returns either the files' content where there
--   was a successful read, otherwise Nothing
safeReadFile :: String -> IO (Maybe String)
safeReadFile path = do
   res <- E.try $ readFile path
   case res of
      Left (e :: E.IOException) -> return Nothing
      Right c -> return $ Just c

-- | Flattens a 2d list (a list of lists) into a single list
flatten :: [[a]] -> [a]
flatten [] = []
flatten (g:gs) = g ++ (flatten gs)

-- | Collects all of the successfully read contents of the read files into an array of
--   targets
allContents :: [String] -> IO [Target]
allContents paths = do
   reads <- safeReadFiles
   let cs = map parseSentinelFile (catMaybes reads)
   return $ flatten $ rights cs
 where safeReadFiles = mapM safeReadFile paths

-- | Builds a list of targets that are applicable to a particular file event
applicableTargets :: Event -> IO [Target]
applicableTargets Modified { maybeFilePath = path } = do
   ts <- allContents locations
   return $ filter (\t -> match (makePattern t) f) ts
 where f = head (maybeToList path)

-- | Performs the actions defined for targets associated to this event
handleFile :: Event -> IO ()
handleFile e = do
   targets <- applicableTargets e
   mapM_ executeCommand targets
   return ()

main :: IO ()
main = do
   cwd <- getCurrentDirectory
   inotify <- initINotify
   wd <- addWatch inotify [Modify] cwd handleFile
   forever $ do
      threadDelay 1000


