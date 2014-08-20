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
--		/a/{`aid}@zzz 
--		translates to 
--		output.zzz = $('a').map(function(x){
--			var set = $(x);
--			assert(set.length == 1);
--			return $(x).attr(id);
--		});

jsGen :: Expr -> JStat
jsGen = \case 
	_ -> [j|var x = 1; foo(x,y);|]
	where 
	s = jsGen

jx :: Expr -> JExpr
jx = \case
	ExSelector _ String
	| ExRef String
	| ExSlot
	| ExBlock Char Char [Expr]
	| ExLeftRec Expr LeftRecRest
	| ExCurriedLeft Operator [Name] [Expr]
	| ExPrefix Operator [Name] [Expr]
	| ExNamed Expr Name

	_->[jE|1|]

codegenTest ast = do
	print $ renderJs $ jsGen ast

main = do
	nCheck<-getArgs
	runSyntaxTests 
		(\n p p' f x -> syntaxTest n p p' f x>>=codegenTest.head) 
		(head $ map read nCheck++[1])
