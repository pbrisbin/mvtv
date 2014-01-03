module System.Directory.SelectFile
    ( selectFile
    , MenuConfig(..)
    , defaultMenuConfig
    ) where

import Data.List (sort)
import Data.Maybe (listToMaybe)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

type Entry = (Int, FilePath)

data MenuConfig = MenuConfig
    { menuColumnMax :: Int
    , menuColumns   :: Int
    , menuPrompt    :: String

    -- | When user input does not index into the list of available
    --   entries, this function will be called with the directory we're
    --   choosing from and the user input. Default implementation is to
    --   just return Nothing.
    , menuOnInvalid :: FilePath -> String -> IO (Maybe FilePath)
    }

defaultMenuConfig :: MenuConfig
defaultMenuConfig = MenuConfig
    { menuColumnMax = 10
    , menuColumns   = 2
    , menuPrompt    = "Select an entry: "
    , menuOnInvalid = \_ _ -> return Nothing
    }

selectFile :: MenuConfig
           -> Bool -- ^ Include hidden files?
           -> FilePath
           -> IO (Maybe FilePath)
selectFile menuConfig showHidden parent = do
    choices <- getEntries showHidden parent

    putStrLn $ parent ++ ":"
    printEntries menuConfig choices

    choice <- prompt $ menuPrompt menuConfig

    case findIn choices =<< maybeRead choice of
        Just entry -> return $ Just $ parent </> entry
        Nothing    -> (menuOnInvalid menuConfig) parent choice

getEntries :: Bool -> FilePath -> IO [Entry]
getEntries showHidden = fmap (toEntries . filterHidden showHidden)
                      . getDirectoryContents

    where
        toEntries :: [FilePath] -> [Entry]
        toEntries = zip [1..] . sort

        filterHidden :: Bool -> [FilePath] -> [FilePath]
        filterHidden True = id
        filterHidden _    = filter visible

        visible :: FilePath -> Bool
        visible ('.':_) = False
        visible _       = True

printEntries :: MenuConfig -> [Entry] -> IO ()
printEntries menuConfig entries = do
    let maxWidth = maximum $ map (length . formatEntry 0) entries

    mapM_ putStrLn $ if length entries <= menuColumnMax menuConfig
        then map (formatEntry maxWidth) entries
        else toColumns maxWidth (menuColumns menuConfig) entries

    putStrLn ""

    where
        toColumns :: Int -> Int -> [(Int, FilePath)] -> [String]
        toColumns maxWidth nColumns es =
            let format l r = formatEntry maxWidth l ++ " " ++ formatEntry maxWidth r
                (ls, rs)   = splitAt (length es `div` nColumns) es

            in zipWith format ls rs

        formatEntry :: Int -> (Int, FilePath) -> String
        formatEntry padTo (i, file) =
            let entry | i < 10    = "   " ++ show i ++ " - " ++ file
                      | i < 100   = "  "  ++ show i ++ " - " ++ file
                      | i < 1000  = " "   ++ show i ++ " - " ++ file
                      | otherwise =          show i ++ " - " ++ file

                padding = replicate (padTo - (length entry)) ' '

            in entry ++ padding

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

findIn :: Eq a => [(a, b)] -> a -> Maybe b
findIn = flip lookup
