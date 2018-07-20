{-# LANGUAGE OverloadedStrings #-}

module OpenSuse.GuessChangeLog ( guessChangeLog, GuessedChangeLog(..) ) where

import OpenSuse.StripSpace

import qualified Control.Foldl as Fold
import Control.Monad.Except
import Data.Algorithm.Diff
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text as Text
import Prelude hiding ( FilePath )
import Turtle hiding ( l, x )

guessChangeLog :: FilePath -> FilePath -> IO GuessedChangeLog
guessChangeLog oldDir = fmap (either id id) . guessChangeLog' oldDir

guessChangeLog' :: FilePath -> FilePath -> IO (Either GuessedChangeLog GuessedChangeLog)
guessChangeLog' oldDir newDir = runExceptT $ do
  oldCLF <- Set.fromList <$> listShell (findChangeLogFiles oldDir)
  newCLF <- Set.fromList <$> listShell (findChangeLogFiles newDir)
  when (all null [oldCLF,newCLF]) (throwError NoChangeLogFiles)
  let clf' = oldCLF `Set.intersection` newCLF
  clf <- case Set.toAscList clf' of
           []    -> throwError (NoCommonChangeLogFiles oldCLF newCLF)
           [clf] -> return clf
           _     -> throwError (MoreThanOneChangeLogFile clf')
  old <- stripSpace <$> liftIO (readTextFile (oldDir </> clf))
  new <- stripSpace <$> liftIO (readTextFile (newDir </> clf))
  let changes    = cleanupEmptyLines (getDiff (Text.lines old) (Text.lines new))
      (top,diff) = span inBoth changes
      (add,foot) = span inSecond diff
      topAddOnly = all inBoth foot
  when (all inBoth changes) (throwError (UndocumentedUpdate clf))
  unless (length top < 10) (throwError (UnmodifiedTopIsTooLarge clf (fromIntegral (length top))))
  unless topAddOnly (throwError (NotJustTopAdditions clf))
  return (GuessedChangeLog clf (stripSpace (Text.unlines (map unDiff add))))

data GuessedChangeLog = GuessedChangeLog FilePath Text
                      | NoChangeLogFiles
                      | UndocumentedUpdate FilePath
                      | NoCommonChangeLogFiles (Set FilePath) (Set FilePath)
                      | MoreThanOneChangeLogFile (Set FilePath)
                      | UnmodifiedTopIsTooLarge FilePath Word
                      | NotJustTopAdditions FilePath
  deriving (Show)

cleanupEmptyLines :: [Diff Text] -> [Diff Text]
cleanupEmptyLines []                                        = []
cleanupEmptyLines (Second t1 : Both "" "" : Second t2 : xs) = Second t1 : Second "" : Second t2 : cleanupEmptyLines xs
cleanupEmptyLines (First  t1 : Both "" "" : First  t2 : xs) = First  t1 : First  "" : First  t2 : cleanupEmptyLines xs
cleanupEmptyLines (x:xs)                                    = x : cleanupEmptyLines xs

inBoth :: Diff a -> Bool
inBoth (Both _ _)   = True
inBoth _            = False

inSecond :: Diff a -> Bool
inSecond (Second _) = True
inSecond _          = False

unDiff :: Diff a -> a
unDiff (First txt)  = txt
unDiff (Both txt _) = txt
unDiff (Second txt) = txt

findChangeLogFiles :: FilePath -> Shell FilePath
findChangeLogFiles dirPath =
  onFiles (grepText changelogFilePattern) (filename <$> ls dirPath)

changelogFilePattern :: Pattern Text
changelogFilePattern = star dot <> asciiCI "change" <> star dot

-- * Utility Functions

listShell :: MonadIO io => Shell a -> io [a]
listShell = flip fold Fold.list
