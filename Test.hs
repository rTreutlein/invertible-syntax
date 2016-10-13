{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, RelaxedPolyRec #-}
{-# LANGUAGE LambdaCase                 #-}
module Example where

import Prelude (Show (..), Read (..), Eq (..), String, Integer,
                map, (++),Maybe (..), Either (..), ($), fst, not, elem,
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

--For text that is both optional and should be parsed into ()
optext :: Syntax delta => String -> delta ()
optext t = (text t <|> text "") <* optSpace --(optext (t++" ") <|> text t <|> text "")

string :: Syntax delta => String -> delta String
string [] = nil <$> pure () <* optext " "
string (x:xs) = cons <$> (subset (== x) <$> token) <*> string xs

p1 = many1 (string "a") <*> string "ab"

mymplus :: Iso alpha gamma -> Iso beta gamma -> Iso (Either alpha beta) gamma
mymplus i j = Iso f g where
    f (Left x) = apply i x
    f (Right x) = apply j x
    g y = case unapply i y of
            Just x -> Just (Left x)
            Nothing -> Right `fmap` unapply j y

{-mapIso :: Iso a b -> Iso [a] [b]
mapIso iso = Iso f g where
    f = mapM apply iso
    g = mapM unapply iso
-}
myid = Iso f g where
    f a = Just a
    g _ = Nothing

iso = Iso f g where
    f a = Just (a,'x')
    g (a,'x') = Just a

ifJustB :: Iso (a,Maybe b) (Either (a,b) a)
ifJustB = Iso (\case {(a,Just b) -> Just $ Left (a,b) ; (a,Nothing) -> Just $  Right a})
              (\case {Left (a,b) -> Just $ (a,Just b) ;  Right a  -> Just $ (a,Nothing)})

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
