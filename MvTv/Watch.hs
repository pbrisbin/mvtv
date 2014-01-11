module MvTv.Watch (initWatch) where

import System.Directory
import System.INotify

initWatch :: IO () -> IO ()
initWatch f = do
    directory <- getCurrentDirectory

    withINotify $ \inotify -> do
        wd <- addWatch inotify [Create] directory $ handler f

        putStrLn $ "Watching " ++ directory ++ ", press Enter to stop."
        getLine

        removeWatch wd

    where
        handler f (Created _ _) = f
