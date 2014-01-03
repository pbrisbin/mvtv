module Main (main) where

import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Directory.SelectFile
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (pathSeparator, joinPath, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    files <- getArgs

    when (null files) $ do
        hPutStrLn stderr "usage: mvtv FILE [, ...]"
        exitFailure

    path <- getPath

    mapM_ (moveTo path) files

getPath :: IO FilePath
getPath =   selectOrCreateFrom -- choose season
        =<< selectOrCreateFrom -- choose show
        =<< getMedia

selectOrCreateFrom :: FilePath -> IO FilePath
selectOrCreateFrom = fmap fromJust . selectFile menuConfig False

    where
        menuConfig :: MenuConfig
        menuConfig = defaultMenuConfig
            { menuPrompt    = "Select an entry or enter a new value: "
            , menuOnInvalid = \parent input ->
                return $ Just $ parent </> input
            }

getMedia :: IO FilePath
getMedia = do
    let def = pathSeparator : joinPath ["mnt", "media", "TV_shows"]

    fmap (fromMaybe def) $ lookupEnv "TV_SHOWS"

moveTo :: FilePath -> FilePath -> IO ()
moveTo directory file = do
    let destination = directory </> takeFileName file

    createDirectoryIfMissing True directory

    putStrLn $ file ++ " -> " ++ destination
    renameFile file destination
