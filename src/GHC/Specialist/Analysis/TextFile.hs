-- |
-- Functions for extracting and analyzing specialist plugin output from a text
-- file containing the specialist notes, separated by newlines.

module GHC.Specialist.Analysis.TextFile where

import GHC.Specialist.Plugin.Types

import Control.Monad
import System.Directory
import Text.Read

 -------------------------------------------------------------------------------
 -- * Folds
 -------------------------------------------------------------------------------

foldMSpecialistNotes
  :: (Monad m, Foldable f)
  => (a -> Maybe SpecialistNote)
  -- ^ How to parse an item of the structure to a 'SpecialistNote'
  -> (b -> SpecialistNote -> m b)
  -- ^ The function to fold with
  -> b
  -- ^ Initial accumulator
  -> f a
  -- ^ The foldable structure
  -> m b
foldMSpecialistNotes mkNote f =
    foldM go
  where
    go acc x =
      case mkNote x of
        Just note -> f acc note
        Nothing -> return acc

foldSpecialistNotes
  :: Foldable f
  => (a -> Maybe SpecialistNote)
  -- ^ How to parse an item of the structure to a 'SpecialistNote'
  -> ((b -> a -> b) -> b -> f a -> b)
  -- ^ The fold to use
  -> (b -> SpecialistNote -> b)
  -- ^ The function to fold with
  -> b
  -- ^ Initial accumulator
  -> f a
  -- ^ The foldable structure
  -> b
foldSpecialistNotes mkNote fold f =
    fold go
  where
    go acc x =
      case mkNote x of
        Just note -> f acc note
        Nothing -> acc

-------------------------------------------------------------------------------
-- * Utilities
-------------------------------------------------------------------------------

specialistNotesFromFile :: FilePath -> IO (Either String [SpecialistNote])
specialistNotesFromFile file = do
    exists <- doesFileExist file
    if not exists then
      return . Left $ "file " ++ file ++ " does not exist"
    else
        Right
      . foldSpecialistNotes readMaybe (foldr . flip) (flip (:)) []
      . lines
      <$> readFile file
