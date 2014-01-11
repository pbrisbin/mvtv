module MvTv.Directory
    ( subDirectories
    , recursiveContents
    , moveFile
    , module System.Directory
    , module System.FilePath
    ) where

import Control.Monad
import System.Directory
import System.FilePath

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

recursiveContents :: FilePath -> IO [FilePath]
recursiveContents directory = do
    names <- fmap (filter (`notElem` [".", ".."]))
           $ getDirectoryContents directory

    paths <- forM names $ \name -> do
        let path = directory </> name
        isDirectory <- doesDirectoryExist path

        if isDirectory
            then recursiveContents path
            else return [path]

    return (concat paths)

moveFile :: FilePath -> FilePath -> IO ()
moveFile from to = do
    let directory = takeDirectory to

    createDirectoryIfMissing True directory
    copyFile from to
    removeFile from
