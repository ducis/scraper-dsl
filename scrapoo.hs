#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables,
	RecordWildCards, ViewPatterns,PatternSynonyms,DeriveDataTypeable #-}

import DSL.Scrapoo.ParseTree
import DSL.Scrapoo.Syntax
import System.Environment

import Language.Javascript.JMacro
import Data.Monoid
import Text.PrettyPrint.Leijen.Text (Doc)
import DSL.Scrapoo.CodegenQQAbbr
import qualified Data.StringMap as SM

--	TODO: XPath
--	TODO Stanford tregex / TGrep2 / cabal package regexp-tries('vertical' pattern)
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
--		SUB-TODO: when applied to the leftmost 'atom', breaks current block.
--				$a-[$b!-$c,$d!-$e]
--				becomes
--				[$a-$b-$c,$a-$d-$e]
--				Or should it be done at typing? Like coercing 'leftmost' string literal to element set
--TODO: referring to names defined later in the source. Needs initialization functions.
--       returns a unary lambda taking possibly a function

type SymbolTable = SM.Map JExpr
data JGenContext = C { 
		cNames::SymbolTable
   }

jsGen :: JGenContext -> Expr -> JStat
jsGen C{..} = \case 
	_ -> [j|var x = 1; foo(x,y);|]
	where 
	s = jsGen

jsx :: JGenContext -> Expr -> ([JStat],Expr)
jsx C{..} = \case
	ExNamed x n -> naming x n

jx :: JGenContext -> Expr -> JExpr
jx C{..} = \case
	ExSelector _ s ->fail"selector" 
	ExRef ((`SM.lookup` cNames)->Just x) -> 
	ExSlot -> fail "slot"
	ExBlock '[' _ xs -> fail "block["
	ExBlock '{' _ xs -> fail "block{"
	ExLeftRec lm rest -> case rest of 
      LrrInfix op ns xs -> namedApp (lm:xs) op ns
      LrrPostfix xs op ns -> namedApp (lm:xs) op ns
      LrrGrouping ns --use context
	ExCurriedLeft op ns xs -> namedApp (ExSlot:xs) op ns
	ExPrefix op ns xs
	_->[jE|1|]

--delete later
data Name = Name Char (Maybe String) String (Maybe String)
	deriving (Eq,Read,Show,Ord)
data Operator 
	= OpSymbolic String 
	| OpAlphabetic String --Including abbreviation
	| OpComposed Char Char [Expr]
	deriving (Eq,Read,Show,Ord)
data Expr 
	= ExSelector Char String
	| ExRef String
	| ExSlot
	| ExBlock Char Char [Expr]
	| ExLeftRec Expr LeftRecRest
	| ExCurriedLeft Operator [Name] [Expr]
	| ExPrefix Operator [Name] [Expr]
	| ExNamed Expr Name
	deriving (Eq,Read,Show,Ord)

data LeftRecRest
	= LrrInfix Operator [Name] [Expr]
	| LrrPostfix [Expr] Operator [Name]
	| LrrGrouping [Name] --essentially a unary postfix operator without arity mark 
	deriving (Eq,Read,Show,Ord)

parseTreeToAST::Expr -> AST
parseTreeToAST = \case
	ExSelector _ s -> ALiteral s
	ExRef s -> ARef s
	ExSlot -> ASlot
	ExBlock k _ xs -> fMany k $ map self xs
	ExLeftRec x lrr -> case lrr of
      LrrInfix Operator [Name] [Expr]
      LrrPostfix [Expr] Operator [Name]
      LrrGrouping [Name] 
	ExCurriedLeft op ns xs
	ExPrefix op ns xs ->
	ExNamed x n
   where
   self = parseTreeToAST
   fMany = \case
      '[' -> AMSimple
      '{' -> AMAggeregate

-- Pattern Match only on the AST type
-- Build AST as simply as possible first then do transformation on it.
data ASTAttachment
   = AA {}
   deriving (Eq,Read,Show,Ord,Typeable,Data)
nAA = AA {}
type AST = (AST',ASTAttachment)
data AST'
   = ALiteral String
	| AApplication [AST] ASTOp
   | ALeftGrouped AST
	| ARef String
	| ABind AST String
	| ALateBind AST String
	| AExtract AST String
	| ASlot
   | AMany ASTMany
   deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTOp   
   = AOSym String
   | AOAlpha String
   | AOMany ASTMany
   deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTMany
   = AMSimple [AST]
   | AMAggeregate [AST]
   deriving (Eq,Read,Show,Ord,Typeable,Data)
-- Do not build explicit AST for now. 
-- use functions below to simulate the structure of ASTs

namedApp::[Expr]->Operator->[Name]->([JStat], SymbolTable, JExpr)
namedApp xs op ns = foldl (application xs op) naming ns

application::[Expr]->Operator->([JStat], SymbolTable, JExpr)
application xs = \case
	OpSymbolic s
	OpAlphabetic s
	OpComposed '{' _ xs 
	OpComposed '[' _ xs

-------------------------------------------------------------

codegenTest ast = do
	print $ renderJs $ jsGen ast

main = do
	nCheck<-getArgs
	runSyntaxTests 
		(\n p p' f x -> syntaxTest n p p' f x>>=codegenTest.head) 
		(head $ map read nCheck++[1])
