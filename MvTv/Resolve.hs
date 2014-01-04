module MvTv.Resolve
    ( resolveShow
    , resolvePath
    ) where

import Data.Char (toLower)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import System.FilePath (takeFileName, (</>))
import Text.Regex.Posix

-- | Expect shows to be well-formed but file not to be. Try to find a
--   value in shows that could apply to file.
resolveShow :: FilePath -> [String] -> Maybe String
resolveShow file = find $ matches file

    where
        matches :: FilePath -> String -> Bool
        matches f s = lower s `isInfixOf` normalize f

        normalize :: FilePath -> FilePath
        normalize = substitute ".-_" ' ' . lower

        lower :: String -> String
        lower = map toLower

        substitute :: [Char] -> Char -> String -> String
        substitute _ _ [] = []
        substitute bad c (x:xs) =
            (if x `elem` bad then c else x):substitute bad c xs

-- | Given a show and potential seasons, return the full path
resolvePath :: String -> [String] -> FilePath -> FilePath
resolvePath s seasons file =
    let fileName = takeFileName file
    in  case getSeason file of
            Nothing     -> s </> fileName
            Just season -> s </> resolveSeason season seasons </> fileName

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
    [ "S([0-9]+)E"
    , "s([0-9]+)e"
    , "[^0-9]([0-9]+)X[0-9]+"
    , "[^0-9]([0-9]+)x[0-9]+"
    ]

    where
        matchFirst :: FilePath -> [String] -> Maybe Int
        matchFirst f = listToMaybe . mapMaybe (matchOne f)

        matchOne :: FilePath -> String -> Maybe Int
        matchOne f = fmap (read) . listToMaybe . reverse . concat . (=~) f
