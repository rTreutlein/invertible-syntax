module Text.Syntax.Printer.Naive where

import Prelude (String,error)
import qualified Prelude as P

import Control.Category
import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Control.Isomorphism.Partial.Unsafe
import Control.Monad (Monad, return, fail, (>>=), liftM2, mplus)

import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.List ((++))
import Data.Maybe (Maybe (Just, Nothing), maybe)

import Text.Syntax.Classes

-- printer

newtype Printer alpha = Printer {print :: alpha -> Maybe String}

wrap p = (Printer p)

printM :: Monad m => Printer alpha -> alpha -> m String
printM p x = maybe (fail "print error") return $ print p x

instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\s -> unapply iso s >>= p)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q
    = Printer (\s -> mplus (p s) (q s))
  Printer p <||> Printer q --See Parser for explanation
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\s -> Nothing)

instance Syntax Printer where
    pure x = Printer (\y ->  if x == y
                               then Just ""
                               else Nothing)
    token = Printer (\t -> Just [t])
    withText (Printer p) = Printer (\(s,_) -> p s)
    ptp _ iso (Printer p2) = Printer (\a -> (unapply iso) P.=<< p2 a)
    withOut p1 _ = p1
