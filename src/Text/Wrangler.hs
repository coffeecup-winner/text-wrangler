{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Wrangler where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
    foldM_ forM readValues actions

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

addAction :: Action a -> W a ()
addAction action = do
    (WData values actions) <- get
    put $ WData values (actions ++ [action])

output :: W T.Text ()
output = addAction $ \text -> do
    T.putStrLn text
    return text

type CharLocation = T.Text -> [Int]

add :: String -> CharLocation -> W T.Text ()
add str loc = addAction $ \text -> do
    let indices = loc text
    let increments = zipWith (-) indices (0:indices)
    let split [] acc t = t:acc
        split (i:is) acc t = split is ((T.take i t):acc) (T.drop i t)
    return . T.concat . intersperse (T.pack str) . reverse $ split increments [] text

before :: String -> CharLocation
before str = \text -> T.length . fst <$> T.breakOnAll (T.pack str) text

after :: String -> CharLocation
after str = \text -> (+ length str) <$> before str text

around :: String -> CharLocation
around str = \text -> merge (before str text) (after str text)
    where merge [] [] = []
          merge (l:ls) (r:rs) = l:r:merge ls rs
