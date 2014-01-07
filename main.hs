module Main (main) where

import MvTv.Resolve

import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do
    files <- getArgs

    when (null files) $ do
        putErr "usage: mvtv FILE [, ...]"
        exitFailure

    media <- getMedia
    titles <- subDirectories media

    forM_ files $ \file -> do
        case resolveShow file titles of
            Nothing -> putErr $ "Unable to resolve show for " ++ file
            Just title -> do
                putStrLn $ "* " ++ takeFileName file
                putStrLn $ "  -> " ++ title

                seasons <- subDirectories $ media </> title
                moveFile file $ media </> resolvePath title seasons file

getMedia :: IO FilePath
getMedia = fmap (fromMaybe defaultMedia) $ lookupEnv "TV_SHOWS"

    where
        defaultMedia :: FilePath
        defaultMedia = pathSeparator
                     : joinPath ["mnt", "media", "TV_shows"]

subDirectories :: FilePath -> IO [FilePath]
subDirectories directory = do
    contents <- getDirectoryContents directory

    fmap (filter visible) $ filterM (isDirectory directory) contents

    where
        visible :: FilePath -> Bool
        visible ('.':_) = False
        visible _       = True

        isDirectory :: FilePath -> FilePath -> IO Bool
        isDirectory parent d = doesDirectoryExist $ parent </> d

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
    let directory = takeDirectory to

    createDirectoryIfMissing True directory
    copyFile from to
    removeFile from

putErr :: String -> IO ()
putErr = hPutStrLn stderr
