

module ApplicativeParsec
  -- Re-export the contents of these modules, for convenience.
  ( module Control.Applicative
  , module Text.ParserCombinators.Parsec
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)

-- Hide Parsec's definitions of some Applicative functions.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))


-- Every Monad is an Applicative
instance Applicative (GenParser s a) where
  pure = return
  (<*>) = ap

-- Every MonadPlus is an Alternative
instance Alternative (GenParser s a) where
  empty = mzero
  (<|>) = mplus
