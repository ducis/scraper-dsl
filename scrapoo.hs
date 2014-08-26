#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, FlexibleInstances, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables,
	RecordWildCards, ViewPatterns, DeriveDataTypeable, LiberalTypeSynonyms,
	StandaloneDeriving, GADTSyntax, GADTs, TypeFamilies, DeriveDataTypeable,
	TypeSynonymInstances, RankNTypes #-}

import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import DSL.Scrapoo.ParseTree
import DSL.Scrapoo.Syntax
import System.Environment
import Text.Groom
import Data.Typeable
-- import Data.Data
import Data.Generics
import Language.Javascript.JMacro
import Data.Monoid
import Text.PrettyPrint.Leijen.Text (Doc)
import DSL.Scrapoo.CodegenQQAbbr
import qualified Data.StringMap as SM
import Data.Maybe
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data
import Data.List.Utils

proom = putStrLn.groom

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
--		SUB-TODO: tree transformation
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
--		 returns a unary lambda taking possibly a function
--TODO: use lens to create reversable mapping from parsetrees to ASTs and between ASTs before and after transformation.

type SymbolTable a = SM.StringMap a
data JGenContext = JGC { 
		cNames::SymbolTable JExpr
	}
rootContext = JGC SM.empty
-- TODO:: Top-level tag (symbols)
--		Or just use AMany as root

data ASTResultType 
	= RTSelector
	| RTSelection
	deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTFlag
	= AFLeft
	| AFRight
   | AFNonLeftmost
	| AFLeftDelegation
	| AFTyped ASTResultType -- use SYB to query type from flags
	| AFBindings [String] --keep sorted with merging
	deriving (Eq,Read,Show,Ord,Typeable,Data)

type AA = [ASTFlag]
aa0 = []

data Taggable t a = T0 a | T1 t a
	deriving (Eq,Read,Show,Ord,Typeable,Data)

type AST a = Taggable AA (ASTExpr a)

data ASTExpr a
	= ASelector String
	| AApplication [AST a] (ASTOp a)
	| ALeftGrouping (AST a)
	| ARef String
	| ABind (AST a) String
	| ALateBind (AST a) String
	| AExtract (AST a) String String
	| ASlot
	| AMany (ASTMany a)
	deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTOp a
	= AOSym String
	| AOAlpha String
	| AOExpr (AST a) -- (ASTMany a)
	deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTMany a
	= AMSimple [AST a]
	| AMAggeregate [AST a]
	deriving (Eq,Read,Show,Ord,Typeable,Data)

jsGen :: JGenContext -> AST a -> JStat
jsGen _ a = [j|var x = 1;|]
-- jsx :: JGenContext -> Expr -> ([JStat],Expr)
-- jx :: JGenContext -> Expr -> JExpr

-- TODO : Type check
-- NOTTODO : (?) break []
-- DONE : eliminate redundant left grouping
-- DONE : mark left-delegation. Keep non-redundant left grouping
-- NOTTODO : (?) eliminate {}
-- TODO : (?) eliminate AExtract
-- DONE : Build local symbol table
--		only {} affects scoping
-- TODO : Mark anonymous vars and collect them

type Rewrite = AST AA -> AST AA

tagAST::Rewrite
tagAST = transform $ \(T0 a)-> T1 aa0 a

rewriteAST::Rewrite
rewriteAST = foldl1 (.) $ reverse rewrites
	
rewrites::[Rewrite]
rewrites = [
	markNonLeftmost,
	eliminateRedundantLeftGrouping,
	markLeftDelegation,
	collectBindings,
	id]

markLeftDelegation = transform $ \self@(T1 aa x) -> (`T1` x) $ ($ aa) $ case x of
	ALeftGrouping _ -> go
	AMany _ -> id
	_ | or [AFLeftDelegation `elem` aa | T1 aa _<-children self] -> go
	_ -> id
	where
	go = (AFLeftDelegation:)

eliminateRedundantLeftGrouping = transform $ \case
	g@(T1 aa' (ALeftGrouping x)) -> case x of
		(T1 _ (AApplication _ _)) -> x
		_ | (AFNonLeftmost `elem` aa') -> x
		_ -> g
	x -> x

markNonLeftmost = transform $ \case
   T1 aa (AApplication (l:rs) op) -> T1 aa (AApplication (l:map f rs) op)
      where
      f (T1 aa x) = T1 (AFNonLeftmost:aa) x
   x -> x

collectBindings = transform $ \self@(T1 aa x ) -> (`T1` x) $ (:aa) $ 
	let
		f = \case
			AFBindings x -> x
			_ -> []
		g = \case
			T1 _ (AMany (AMAggeregate _)) -> []
			T1 aa _->map f aa
		collect added = AFBindings $ foldl merge added $ concat $ map g $ children self
	in case x of 
		ABind x1 s -> collect [s]
		-- AMany (AMAggeregate _) -> AFBindings []
		-- AApplication _ (AOExpr (AMAggeregate _)) -> AFBindings []
		_ -> collect []

--typing0 = transform $ \(T1 x aa) -> T1 x $ (\z->aa{aaType=z}) $ case x of
--	ASelector s -> RTSelector
--	_ -> RTSelection



simplify0::AST () -> AST ()
simplify0 = everywhere' $ mkT $ \case
	x -> x::AST ()


type AST0 = AST AA
parseTreeToAST::Expr -> AST0
parseTreeToAST = \case
	ExSelector _ s -> T0 $ ASelector s
	ExRef s -> T0 $ ARef s
	ExSlot -> T0 $ ASlot
	ExBlock k _ xs -> fMany k $ selfs xs
	ExLeftRec x lrr -> case lrr of
		LrrInfix op ns xs -> fNmdApp (x:xs) op ns
		LrrPostfix xs op ns -> fNmdApp (x:xs) op ns
		LrrGrouping ns -> fNs ns $ T0 $ ALeftGrouping $ self x
	ExCurriedLeft op ns xs -> fNmdApp (ExSlot:xs) op ns
	ExPrefix op ns xs -> fNmdApp xs op ns
	ExNamed x n -> fName (self x) n
	where
	-- aa = (,nAA)
	fName::AST0 -> Name -> AST0
	fName ast (Name ch l n r) = f2 $ f1 (T0 $ f0 ast n)
		where
		f0 = case ch of
			'@' -> ABind
			'#' -> ALateBind
		[f1,f2] = map (maybe id (\f a->T0 $ AExtract a f n)) [l,r]
	fNmdApp::[Expr] -> Operator -> [Name] -> AST0
	fNmdApp xs op ns = fNs ns $ T0 $ AApplication (selfs xs) (fOp op)
	fNs::[Name]->AST0->AST0
	fNs ns ast = foldl fName ast ns
	self = parseTreeToAST
	selfs = map self
	fMany = ((T0 . AMany).). \case
		'[' -> AMSimple
		'{' -> AMAggeregate
	fOp = \case
		OpSymbolic s -> AOSym s 
		OpAlphabetic s -> AOAlpha s
		OpComposed k _ xs -> AOExpr $ fMany k $ selfs xs

-- Pattern Match only on the AST type
-- Build AST as simply as possible first then do transformation on it.

-------------------------------------------------------------

codegenTest ast = do
	print $ renderJs $ jsGen rootContext ast

astTest expr = do
	let a = rewriteAST $ tagAST $ parseTreeToAST expr
	proom a
	return a

main = do
	nCheck<-getArgs
	--TODO: diff AST between rewrites
	runSyntaxTests 
		(\n p p' f x -> syntaxTest n p p' f x >>= astTest.head >>= codegenTest ) 
		(head $ map read nCheck++[1])

--type AA = ASTAttachment
--data ASTAttachment = AA {
--	aaFlags::[ASTFlag]
--	}
--	deriving (Eq,Read,Show,Ord,Typeable,Data)
-- aa0 = AA { aaFlags = [] }

