#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, FlexibleInstances, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables,
	RecordWildCards, ViewPatterns, DeriveDataTypeable, LiberalTypeSynonyms,
	StandaloneDeriving, GADTSyntax, GADTs, TypeFamilies, DeriveDataTypeable,
	TypeSynonymInstances #-}

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

type SymbolTable = SM.StringMap JExpr
data JGenContext = C { 
		cNames::SymbolTable
	}

type AA = ASTAttachment
data ASTAttachment
	= AA {}
	deriving (Eq,Read,Show,Ord,Typeable,Data)
nAA = AA {}

data Taggable t a = T0 a | T1 t a
	deriving (Eq,Read,Show,Ord,Typeable,Data)

type AST f = Taggable AA (ASTExpr f)

data ASTExpr f
	= ALiteral String
	| AApplication [AST f] (ASTOp f)
	| ALeftGrouping (AST f)
	| ARef String
	| ABind (AST f) String
	| ALateBind (AST f) String
	| AExtract (AST f) String String
	| ASlot
	| AMany (ASTMany f)
	deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTOp f
	= AOSym String
	| AOAlpha String
	| AOMany (ASTMany f)
	deriving (Eq,Read,Show,Ord,Typeable,Data)
data ASTMany f
	= AMSimple [AST f]
	| AMAggeregate [AST f]
	deriving (Eq,Read,Show,Ord,Typeable,Data)

-- TODO : Type check
-- TODO : eliminate left grouping
-- TODO : (?) eliminate {}
-- TODO : (?) eliminate Bind and LateBind

--typecheck::AST () -> AST AA
--typecheck = every

simplify0::AST () -> AST ()
simplify0 = everywhere' $ mkT $ \case
	x -> x::AST ()

-- jsGen :: JGenContext -> Expr -> JStat
-- jsx :: JGenContext -> Expr -> ([JStat],Expr)
-- jx :: JGenContext -> Expr -> JExpr

type AST0 = AST AA
parseTreeToAST::Expr -> AST0
parseTreeToAST = \case
	ExSelector _ s -> T0 $ ALiteral s
	ExRef s -> T0 $ ARef s
	ExSlot -> T0 $ ASlot
	ExBlock k _ xs -> T0 $ AMany $ fMany k $ selfs xs
	ExLeftRec x lrr -> case lrr of
		LrrInfix op ns xs -> fNmdApp (x:xs) op ns
		LrrPostfix xs op ns -> fNmdApp (x:xs) op ns
		LrrGrouping ns -> fNs ns $ self x
	ExCurriedLeft op ns xs -> fNmdApp (ExSlot:xs) op ns
	ExPrefix op ns xs -> fNmdApp xs op ns
	ExNamed x n -> fName (self x) n
	where
	-- aa = (,nAA)
	fName ast (Name ch l n r) = f2 $ f1 (T0 $ f0 ast n)
		where
		f0 = case ch of
			'@' -> ABind
			'#' -> ALateBind
		[f1,f2] = map (maybe id (\f a->T0 $ AExtract a f n)) [l,r]
			
	fNmdApp xs op ns = fNs ns $ T0 $ AApplication (selfs xs) (fOp op)
	fNs::[Name]->AST0->AST0
	fNs ns ast = foldl fName ast ns
	self = parseTreeToAST
	selfs = map self
	fMany = \case
		'[' -> AMSimple
		'{' -> AMAggeregate
	fOp = \case
		OpSymbolic s -> AOSym s 
		OpAlphabetic s -> AOAlpha s
		OpComposed k _ xs -> AOMany $ fMany k $ selfs xs

-- Pattern Match only on the AST type
-- Build AST as simply as possible first then do transformation on it.

-----------------------------------------------------------------
{-
namedApp::[Expr]->Operator->[Name]->([JStat], SymbolTable, JExpr)

application::[Expr]->Operator->([JStat], SymbolTable, JExpr)
-}
-------------------------------------------------------------

{- codegenTest ast = do
	print $ renderJs $ jsGen ast -}

astTest expr = do
	proom $ parseTreeToAST expr

main = do
	nCheck<-getArgs
	runSyntaxTests 
		(\n p p' f x -> syntaxTest n p p' f x>>=astTest.head) 
		(head $ map read nCheck++[1])
