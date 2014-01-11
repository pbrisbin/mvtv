module Main (main) where

import MVTV.Directory
import MVTV.Resolve
import MVTV.Watch

import Control.Monad
import Data.Maybe
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    media <- getMedia

    if args == ["--watch"]
        then initWatch $ processFiles media
        else processFiles media

getMedia :: IO FilePath
getMedia = fmap (fromMaybe defaultMedia)
         $ lookupEnv "TV_SHOWS"

    where
        defaultMedia :: FilePath
        defaultMedia = pathSeparator
                     : joinPath ["mnt", "media", "TV_shows"]

findFiles :: IO [FilePath]
findFiles = do
    directory <- getCurrentDirectory

    fmap (filter isMedia) $ recursiveContents directory

    where
        isMedia :: FilePath -> Bool
        isMedia file = (takeExtension file) `elem`
            [".mkv", ".avi", ".mp4", ".mpeg", ".mpg", ".mov"]

processFiles :: FilePath -> IO ()
processFiles media = do
    files <- findFiles
    titles <- subDirectories media

    forM_ files $ \file -> do
        case resolveShow file titles of
            Nothing -> return ()
            Just title -> do
                putStrLn $ "* " ++ takeFileName file
                putStrLn $ "  -> " ++ title

                seasons <- subDirectories $ media </> title
                moveFile file $ media </> resolvePath title seasons file
