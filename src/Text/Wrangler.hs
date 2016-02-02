{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Wrangler where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import qualified System.FilePath.Find as F

class Selector s a b | s a -> b where
    select :: s -> a -> IO [b]

instance Selector [Char] Folder File where
    select pattern (Folder name) = do
        files <- F.find F.always (F.fileName F.~~? pattern) name
        return $ File <$> files

data WData a = WData { dataValues :: IO [a]
                     , dataActions :: [Action a]
                     }

type W a = StateT (WData a) Identity

data Folder = Folder { folderName :: String
                     } deriving (Eq, Show)

data File = File { fileName :: String
                 } deriving (Eq, Show)

data LineSelector = LineSelector

instance Selector LineSelector File T.Text where
    select LineSelector (File name) = do
        text <- T.readFile name
        return $ T.lines text

data From = From

type Action a = a -> IO a

wrangle :: W a () -> IO ()
wrangle w = do
    let (WData values actions) = execState w (WData (return []) [])
    readValues <- values
    sequence $ (flip (<$>)) actions $ \action -> do
        mapM action readValues
    return ()

line :: LineSelector
line = LineSelector

every :: Selector s a b => s -> From -> W a () -> W b ()
every s From w = do
    let (WData values _) = execState w (WData (return []) [])
    let newValues = do
            readValues <- values
            selected <- mapM (select s) readValues
            return $ concat selected
    put $ WData newValues []

from :: From
from = From

currentFolder :: W Folder ()
currentFolder = do
    put $ WData (return [Folder "."]) []

output :: W T.Text ()
output = do
    let action = \str -> do
            T.putStrLn str
            return str
    (WData values actions) <- get
    put $ WData values (actions ++ [action])
