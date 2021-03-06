module Text.Syntax.Classes where

import Prelude (String)

import Control.Isomorphism.Partial (IsoFunctor)
import Control.Isomorphism.Partial.Unsafe
import Data.Eq (Eq)
import Data.Char (Char)

infixl 3 <|>
infixl 3 <||>
infixr 6 <*>

class ProductFunctor f where
  (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alternative f where
  (<|>)  :: f alpha -> f alpha -> f alpha
  (<||>) :: f alpha -> f alpha -> f alpha --Optimaization
  empty  :: f alpha                       --See Parser Implementation for Detais

class (IsoFunctor delta, ProductFunctor delta, Alternative delta)
   => Syntax delta where
  -- (<$>)   ::  Iso alpha beta -> delta alpha -> delta beta
  -- (<*>)   ::  delta alpha -> delta beta -> delta (alpha, beta)
  -- (<|>)   ::  delta alpha -> delta alpha -> delta alpha
  -- empty   ::  delta alpha
  pure      ::  Eq alpha => alpha -> delta alpha
  token     ::  delta Char
  withText  ::  delta alpha -> delta (alpha,String)
  withOut   ::  Eq beta => delta alpha -> delta beta -> delta alpha
  ptp       ::  delta alpha -> Iso String String -> delta beta -> delta beta
