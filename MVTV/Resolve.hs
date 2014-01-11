module MVTV.Resolve
    ( resolveShow
    , resolvePath
    ) where

import Data.Char (toLower)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.FilePath (takeFileName, (</>))
import Text.Regex.Posix

resolveShow :: FilePath -> [String] -> Maybe String
resolveShow file = find $ matches file

    where
        matches :: FilePath -> String -> Bool
        matches f title = normalize title `isInfixOf` normalize f

        normalize :: FilePath -> FilePath
        normalize = substitute "._-" ' ' . lower

        lower :: String -> String
        lower = map toLower

        substitute :: [Char] -> Char -> String -> String
        substitute _ _ [] = []
        substitute bad c (x:xs) =
            (if x `elem` bad then c else x):substitute bad c xs

resolvePath :: String -> [String] -> FilePath -> FilePath
resolvePath title seasons file =
    let fileName = takeFileName file
    in  case getSeason file of
            Nothing     -> title </> fileName
            Just season -> title </> resolveSeason season seasons </> fileName

resolveSeason :: Int -> [String] -> String
resolveSeason season = fromMaybe (defaultSeason season)
                     . find (matches season)

    where
        matches :: Int -> String -> Bool
        matches i str =
            let one = show i
                ten = show i ++ "0"
            in one `isInfixOf` str && (not $ ten `isInfixOf` str)

        defaultSeason :: Int -> String
        defaultSeason i = "season_" ++ show i

getSeason :: FilePath -> Maybe Int
getSeason file = matchFirst file
    -- N.B. patterns much a) have exactly one capture group and b) match
    -- and capture a parsable int or not match at all.
    [ "S([0-9]+)E"
    , "s([0-9]+)e"
    , "[^0-9]([0-9]+)X[0-9]+"
    , "[^0-9]([0-9]+)x[0-9]+"
    ]

    where
        matchFirst :: FilePath -> [String] -> Maybe Int
        matchFirst f = listToMaybe . mapMaybe (matches f)

        matches :: FilePath -> String -> Maybe Int
        matches f = fmap (read) . listToMaybe . reverse . concat . (=~) f
