{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenSuse.GuessChangeLog ( guessChangeLog, GuessedChangeLog(..) ) where

import qualified Control.Foldl as Fold
import Control.Monad.Except
import Data.Algorithm.Diff
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Text as Text
import Prelude hiding ( FilePath )
import Turtle hiding ( l, x )

guessChangeLog :: FilePath -> FilePath -> IO (Either GuessedChangeLog Text)
guessChangeLog oldDir newDir = runExceptT $ do
  oldCLF <- Set.fromList <$> listShell (findChangeLogFiles oldDir)
  newCLF <- Set.fromList <$> listShell (findChangeLogFiles newDir)
  when (all null [oldCLF,newCLF]) (throwError NoChangeLogFiles)
  let clf' = oldCLF `Set.intersection` newCLF
  clf <- case Set.toAscList clf' of
           []    -> throwError (NoCommonChangeLogFiles oldCLF newCLF)
           [clf] -> return clf
           _     -> throwError (MoreThanOneChangeLogFile clf')
  (oec,old) <- shellStrict (format ("git stripspace < "%fp) (oldDir </> clf)) empty
  (nec,new) <- shellStrict (format ("git stripspace < "%fp) (newDir </> clf)) empty
  unless (all (== ExitSuccess) [oec,nec]) $
    -- TODO: Throw a proper exception here, or even don't even rely on git-stripspace.
    die (format ("git stripspace failed with "%w%"\n") oec)
  let changes    = cleanupEmptyLines (getDiff (Text.lines old) (Text.lines new))
      (top,diff) = span inBoth changes
      (add,foot) = span inSecond diff
      topAddOnly = all inBoth foot
  when (all inBoth changes) (throwError (UndocumentedUpdate clf))
  unless (length top < 10) (throwError (UnmodifiedTopIsTooLarge clf (fromIntegral (length top))))
  unless topAddOnly (throwError (NotJustTopAdditions clf))
  return (Text.strip (Text.unlines (map unDiff add)))

data GuessedChangeLog = NoChangeLogFiles
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
