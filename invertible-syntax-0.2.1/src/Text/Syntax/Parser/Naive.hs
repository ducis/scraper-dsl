module Text.Syntax.Parser.Naive where

import Prelude (String)

import Control.Category ()
import Control.Isomorphism.Partial (IsoFunctor, (<$>), apply)
import Control.Monad (Monad, return, fail, (>>=))

import Data.List ((++))
import Data.Maybe (Maybe (Just))

import Text.Syntax.Classes (ProductFunctor, Alternative, Syntax, (<*>), (<|>), empty, pure, token)     			

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

instance ProductFunctor Parser where
  Parser p <*> Parser q 
    = Parser (\s ->  [  ((x, y), s'') 
                     |  (x,  s')   <- p  s
                     ,  (y,  s'')  <- q  s' ])

instance Alternative Parser where
  Parser p <|> Parser q 
    = Parser (\s -> p s ++ q s)
  empty = Parser (\s -> [])

instance Syntax Parser where
  pure x  =  Parser (\s -> [(x, s)])
  token   =  Parser f where
    f []      =  []
    f (t:ts)  =  [(t, ts)] 
