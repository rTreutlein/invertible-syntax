module Text.Syntax.Parser.Naive where

import Prelude (String,(-),length,take,(==))
import qualified Prelude as P

import Control.Category ()
import Control.Isomorphism.Partial (IsoFunctor, (<$>), apply)
import Control.Isomorphism.Partial.Unsafe
import Control.Monad (Monad, return, (>>=))

import Data.List ((++))
import Data.Maybe

import Text.Syntax.Classes

-- parser

newtype Parser alpha
  = Parser (String -> [(alpha, String)])


rawparse :: Parser alpha -> String -> [(alpha,String)]
rawparse (Parser p) s = p s

parse :: Parser alpha -> String -> [alpha]
parse (Parser p) s = [ x | (x, "") <- p s ]

parseM :: Monad m => Parser alpha -> String -> m alpha
parseM p s
  =  case parse p s of
       []        ->  P.error "parse error"
       [result]  ->  return result
       _         ->  P.error "ambiguous input"

instance IsoFunctor Parser where
  iso <$> Parser p
    = Parser (\s ->  [  (y, s')
                     |  (x, s')  <-  p s
                     ,  Just y   <-  [apply iso x] ])

instance ProductFunctor Parser where
  Parser p <*> Parser q
    = Parser (\s ->  [  ((x, y), s'')
                     |  (x,  s')   <- p  s
                     ,  (y,  s'')  <- q  s' ])

instance Alternative Parser where
  Parser p <|> Parser q
    = Parser (\s -> case p s of
                        [] -> q s
                        a  -> a -- ++ (q s)
             )
  Parser p <||> Parser q            --This considers both posibilites
    = Parser (\s -> (p s) ++ (q s)) --Usefull incase one way leads into a dead end
  empty = Parser (\s -> [])         --but to slow to always use.

instance Syntax Parser where
    pure x  =  Parser (\s -> [(x, s)])
    token   =  Parser f where
      f []      =  []
      f (t:ts)  =  [(t, ts)]
    withText (Parser p)
            = Parser (\s -> case p s of
                            [] -> []
                            a  -> P.map (getText s) a
                     )
        where getText s (e,r) = ((e,_getText s r),r)
              _getText s r = take (lengthDiff s r) s
              lengthDiff a b = (length a) - (length b)

    ptp parser1 iso (Parser p2) = Parser (\s -> P.concatMap p2 (filterd s))
            where mapfunc = myConcat P.. (mapFst (apply iso)) P.. (mapFst P.snd)
                  morphed s = P.map mapfunc (p1 s)
                  filterd s = P.map (\(Just a) -> a) P.$ P.filter isJust (morphed s)
                  (Parser p1) = withText parser1
    withOut (Parser p1) (Parser p2) =
        Parser (\s -> let parses = p1 s
                      in P.foldr (\(x,s') ls -> if p2 s' == []
                                                   then (x,s'):ls
                                                   else ls)
                                 [] parses)

mapFst f (a,b) = (f a,b)
mapSnd f (a,b) = (a,f b)

myConcat (Just a,b) = Just (a ++ b)
myConcat (Nothing,b) = Nothing
