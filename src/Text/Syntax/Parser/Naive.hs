module Text.Syntax.Parser.Naive where

import Prelude (String,(-),length,take)
import qualified Prelud as P

import Control.Category ()
import Control.Isomorphism.Partial (IsoFunctor, (<$>), apply)
import Control.Monad (Monad, return, fail, (>>=))

import Data.List ((++))
import Data.Maybe (Maybe (Just))

import Text.Syntax.Classes (ProductFunctor, Alternative, Syntax, Choice, (<*>), (<|>),  empty, try, pure, token, withText)

-- parser

newtype Parser alpha
  = Parser (String -> [(alpha, String)])

parse :: Parser alpha -> String -> [alpha]
parse (Parser p) s = [ x | (x, "") <- p s ]

parseM :: Monad m => Parser alpha -> String -> m alpha
parseM p s
  =  case parse p s of
       []        ->  fail "parse error"
       [result]  ->  return result
       _         ->  fail "ambiguous input"

instance IsoFunctor Parser where
  iso <$> Parser p
    = Parser (\s ->  [  (y, s')
                     |  (x, s')  <-  p s
                     ,  Just y   <-  [apply iso x] ])

Parser p <.> iso
    = (\s -> p $ apply iso x)

instance ProductFunctor Parser where
  Parser p <*> Parser q
    = Parser (\s ->  [  ((x, y), s'')
                     |  (x,  s')   <- p  s
                     ,  (y,  s'')  <- q  s' ])

instance Alternative Parser where
  Parser p <|> Parser q
    = Parser (\s -> case p s of
                        [] -> q s
                        a  -> a)
  empty = Parser (\s -> [])

{-instance Choice Parser where
    try (Parser p) (Parser q)
    = Parser (\s -> case p s of
        [] -> []
                        a  -> q s)
-}

instance Monad Parser where

    (Parser a) -> (a -> Parser b) -> Parser b
    (Parser p) >>= f
        = Parser (\s -> case p s of
                            [] -> []
                            ls -> f ls)

    (Parser String) -> Iso String String -> Parser Atom -> Parser Atom
    (Parser p1) iso (Parser p2) = Parser (\s -> concatMap p2 $ map mapfunc p1 s)
        where mapfunc = myappend . (mapFst $ apply iso)

    (Printer String) -> Iso String String -> Printer Atom -> Printer Atom
    _ iso (Printer p2) = Printer (\a -> (unapply iso) P.<$> p2 a)

mapFst f (a,b) = (f a,b)

myappend (a,b) = a ++ b

instance Syntax Parser where
  pure x  =  Parser (\s -> [(x, s)])
  token   =  Parser f where
    f []      =  []
    f (t:ts)  =  [(t, ts)]
  withText (Parser p)
          = Parser (\s -> case p s of
                          [] -> []
                          [(e,r)] -> let n = (length s) - (length r)
                                     in [((e,take n s),r)]
                   )
