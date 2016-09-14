module Text.Syntax.Printer.Naive where

import Prelude (String)

import Control.Category ()
import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Control.Monad (Monad, return, fail, (>>=), liftM2, mplus)

import Data.Eq (Eq ((==)))
import Data.Function (($))
import Data.List ((++))
import Data.Maybe (Maybe (Just, Nothing), maybe)

import Text.Syntax.Classes (ProductFunctor, Alternative, Syntax, Choice, (<*>), (<|>),  empty, try, pure, token, withText)

-- printer

newtype Printer alpha = Printer (alpha -> Maybe String)

print :: Printer alpha -> alpha -> Maybe String
print (Printer p) x = p x

printM :: Monad m => Printer alpha -> alpha -> m String
printM p x = maybe (fail "print error") return $ print p x

instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\b -> unapply iso b >>= p)

Printer p <.> iso
    = Printer (\b -> fmap (unapply iso) p s)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\s -> Nothing)

instance Choice Printer where
  try p q
    = q

instance Syntax Printer where
    pure x = Printer (\y ->  if x == y
                               then Just ""
                               else Nothing)
    token = Printer (\t -> Just [t])
    withText (Printer p) = Printer (\(s,_) -> p s)
    ptp :: (Printer String) -> Iso String String -> Printer Atom -> Printer Atom
    ptp _ iso (Printer p2) = Printer (\a -> (unapply iso) P.<$> p2 a)


