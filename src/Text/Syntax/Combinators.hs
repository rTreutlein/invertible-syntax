module Text.Syntax.Combinators
  (  -- * Lexemes
     text
  ,  comma
  ,  dot
     -- * Repetition
  ,  many
  ,  many1
  ,  sepBy
  ,  chainl1
     -- * Sequencing
  ,  (*>)
  ,  (<*)
  ,  between
     -- * Alternation
  ,  (<+>)
  ,  optional
  ,  optional2
     -- * Whitespace
  ,  skipSpace
  ,  sepSpace
  ,  optSpace
     -- XXX This shouldn't be here
  ,  (&&&)) where

import Prelude (String,(+))
import qualified Prelude  as P

import Control.Category ((.))
import Control.Isomorphism.Partial.Constructors (nothing, just, nil, cons, left, right)
import Control.Isomorphism.Partial.Derived (foldl)
import Control.Isomorphism.Partial.Prim
import Control.Isomorphism.Partial.Unsafe

import Data.Maybe (Maybe)
import Data.Either (Either)

import Text.Syntax.Classes

(&&&) :: Iso alpha beta -> Iso alpha gamma -> Iso alpha (beta,gamma)
i &&& j = Iso f g where
    f a =  (,) P.<$> apply i a P.<*> apply j a
    g (b,g) = unapply i b

-- derived combinators
many :: Syntax delta => delta alpha -> delta [alpha]
many p = (cons <$> p <*> many p) <|> (nil <$> pure () )

many1 :: Syntax delta => delta alpha -> delta [alpha]
many1 p = cons <$> p <*> many p

infixl 4 <+>

(<+>) :: Syntax delta => delta alpha -> delta beta -> delta (Either alpha beta)
p <+> q = (left <$> p) <|> (right <$> q)

-- | `text` parses\/prints a fixed text and consumes\/produces a unit value.
text :: Syntax delta => String -> delta ()
text []      =    pure ()
text (c:cs)  =    inverse (element ((), ()))
             <$>  (inverse (element c) <$> token)
             <*>  text cs

-- | This variant of `<*>` ignores its left result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(*>) :: Syntax delta => delta () -> delta alpha -> delta alpha
p *> q = inverse unit . commute <$> p <*> q

-- | This variant of `<*>` ignores its right result.
-- In contrast to its counterpart derived from the `Applicative` class, the ignored
-- parts have type `delta ()` rather than `delta beta` because otherwise information relevant
-- for pretty-printing would be lost.

(<*) :: Syntax delta => delta alpha -> delta () -> delta alpha
p <* q = inverse unit <$> p <*> q

-- | The `between` function combines `*>` and `<*` in the obvious way.
between :: Syntax delta => delta () -> delta () -> delta alpha -> delta alpha
between p q r = p *> r <* q

-- | The `chainl1` combinator is used to parse a
-- left-associative chain of infix operators.
chainl1 :: Syntax delta => delta alpha -> delta beta -> Iso (alpha, (beta, alpha)) alpha -> delta alpha
chainl1 arg op f
  = foldl f <$> arg <*> many (op <*> arg)

optional :: Syntax delta => delta alpha -> delta (Maybe alpha)
optional x  = just <$> x <|> nothing <$> text ""

--Always parses a Nothing
optional2 :: Syntax delta => delta alpha -> delta (Maybe alpha)
optional2 x  = just <$> x <||> nothing <$> text ""

sepBy :: Syntax delta => delta alpha -> delta () -> delta [alpha]
sepBy x sep
  = cons <$> x <*> many (sep *> x)
  <|>  nil <$> text ""

comma :: Syntax delta => delta ()
comma = text ","

dot :: Syntax delta => delta ()
dot = text "."


-- Expressing whitespace
-- ---------------------
--
-- Parsers and pretty printers treat whitespace
-- differently. Parsers
-- specify where whitespace is allowed or required to occur, while
-- pretty printers specify how much whitespace is to be inserted at
-- these locations. To account for these different roles of
-- whitespace, the following three syntax descriptions provide
-- fine-grained control over where whitespace is allowed, desired or
-- required to occur.

-- | `skipSpace` marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing.

skipSpace  ::  Syntax delta => delta ()
skipSpace  =   ignore []    <$>  many (text " ")

-- | `optSpace` marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a
-- single space character while printing.

optSpace  ::  Syntax delta => delta ()
optSpace  =   ignore [()]  <$>  many (text " ")

-- | `sepSpace` marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing,
-- and produces a single space character while printing.

sepSpace  ::  Syntax delta => delta ()
sepSpace  =   text " " <* skipSpace

