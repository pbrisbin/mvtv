module MVTV.Watch (initWatch) where

import System.Directory
import System.INotify

initWatch :: IO () -> IO ()
initWatch action = do
    directory <- getCurrentDirectory

    withINotify $ \inotify -> do
        wd <- addWatch inotify [Create] directory $ handler action

        putStrLn $ "Watching " ++ directory ++ ", press Enter to stop."
        _ <- getLine

        removeWatch wd

    where
        handler :: IO () -> (Event -> IO ())
        handler a (Created _ _) = a
        handler _ _ = return ()
