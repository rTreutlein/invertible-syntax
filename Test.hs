{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
module Example where

import Prelude (Show (..), Read (..), Eq (..), String, Integer,
                map, (++), Maybe (..), ($), fst, not, elem,
                notElem, reads, Char)

import Control.Category (id, (.))

import Control.Monad (mplus)

import Data.Char (isLetter, isDigit)

--import qualified Text.ParserCombinators.Parsec as Parsec

import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.TH
import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Text.Syntax
import Text.Syntax.Parser.Naive
import Text.Syntax.Printer.Naive

import Control.Monad.Trans.Reader
import Control.Monad

data Expression
    =  Variable String
    |  Literal Integer
    |  BinOp Expression Operator Expression
    |  IfZero Expression Expression Expression
  deriving (Show, Eq)

data Operator
    =  AddOp
    |  MulOp
  deriving (Show, Eq)
$(defineIsomorphisms ''Expression)
$(defineIsomorphisms ''Operator)

keywords = ["ifzero", "else"]

letter, digit :: Syntax delta => delta Char
letter  =  subset isLetter <$> token
digit   =  subset isDigit <$> token

identifier
  = subset (`notElem` keywords) . cons <$>
      letter <*> many (letter <|> digit)

keyword :: Syntax delta => String -> delta ()
keyword s = inverse right <$> (identifier <+> text s)

test :: Syntax delta => Reader String (delta ())
test = do
    word <- ask
    return (text word)

p1 :: Syntax delta => delta Char
p1 = inverse (ignore 'a') <$> text "r"

p2 :: Syntax delta => delta Char
p2 = token

p3 :: Syntax delta => delta Char
p3 = p1 <|> p2

{-    integer :: Syntax delta => delta Integer
integer = Iso read' show' <$> many digit where
    read' s  =  case [ x | (x, "") <- reads s ] of
        [] -> Nothing
                (x : _) -> Just x

  show' x  =  Just (show x)

parens = between (text "(") (text ")")

ops  =    mulOp  <$>  text "*"
     <|>  addOp  <$>  text "+"

spacedOps = between optSpace optSpace ops

priority :: Operator -> Integer
priority  MulOp  =  1
priority  AddOp  =  2

expression = exp 2 where

  exp 0  =    literal    <$>  integer
         <|>  variable   <$>  identifier
         <|>  ifZero     <$>  ifzero
         <|>  parens (skipSpace *> expression <* skipSpace)
  exp 1  =    chainl1  (exp 0)  spacedOps  (binOpPrio 1)
  exp 2  =    chainl1  (exp 1)  spacedOps  (binOpPrio 2)

  ifzero  =    keyword "ifzero"
          *>   optSpace  *>  parens (expression)
          <*>  optSpace  *>  parens (expression)
          <*>  optSpace  *>  keyword "else"
          *>   optSpace  *>  parens (expression)

  binOpPrio n
    = binOp . subset (\(x, (op, y)) -> priority op == n)-}
