#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}

import DSL.Scrapoo.ParseTree
import DSL.Scrapoo.Syntax
import System.Environment

import Language.Javascript.JMacro
import Data.Monoid
import Text.PrettyPrint.Leijen.Text (Doc)
import DSL.Scrapoo.CodegenQQAbbr

--	TODO: XPath
--	TODO Stanford tregex / TGrep2
--TODO: Indentation after pretty-printing
--TODO: Parallelize tests
--TODO: Type checker
--TODO: currying $$``[__``a] $$$```[___```b]
--		$$[``a] $$$[```b]
--TODO: numeric indexing and ranges
--TODO: code generation		CURRENT
--		SUB-TODO: operator table
--		SUB-TODO: assertion(x.length == 1);
--      SUB-TODO: tree transformation
--		/a/{`aid}@zzz 
--		translates to 
--		output.zzz = $('a').map(function(x){
--			var set = $(x);
--			assert(set.length == 1);
--			return $(x).attr(id);
--		});

data JGenContext = C { 
		
   }

jsGen :: JGenContext -> Expr -> JStat
jsGen = \case 
	ExNamed Expr Name
	_ -> [j|var x = 1; foo(x,y);|]
	where 
	s = jsGen

jx :: JGenContext -> Expr -> JExpr
jx = \case
	ExSelector _ s -> 
	ExRef s -> 
	ExSlot -> fail "slot"
	ExBlock '[' _ xs -> fail "block["
	ExBlock '{' _ xs -> fail "block{"
	ExLeftRec lm rest -> case rest of 
      LrrInfix op ns xs -> namedApp (lm:xs) op ns
      LrrPostfix xs op ns -> namedApp (lm:xs) op ns
      LrrGrouping ns --use context
	ExCurriedLeft op ns xs -> namedApp (ExSlot:xs) op ns
	ExPrefix op ns xs
	| ExNamed Expr Name
	_->[jE|1|]

namedApp xs op ns = foldl (application xs op) naming ns

application xs = \case
	= OpSymbolic String 
	| OpAlphabetic String --Including abbreviation
	| OpComposed Char Char [Expr]

codegenTest ast = do
	print $ renderJs $ jsGen ast

main = do
	nCheck<-getArgs
	runSyntaxTests 
		(\n p p' f x -> syntaxTest n p p' f x>>=codegenTest.head) 
		(head $ map read nCheck++[1])
