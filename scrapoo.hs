#!/usr/bin/runghc
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables,
	RecordWildCards, ViewPatterns, DeriveDataTypeable, LiberalTypeSynonyms #-}

import DSL.Scrapoo.ParseTree
import DSL.Scrapoo.Syntax
import System.Environment
import Text.Groom
import Data.Typeable
import Data.Data
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
{-
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
-}

type AST0 = AST (Int->Int)
parseTreeToAST::Expr -> AST0
parseTreeToAST = \case
	ExSelector _ s -> ALiteral s
	ExRef s -> ARef s
	ExSlot -> ASlot
	ExBlock k _ xs -> AMany $ fMany k $ selfs xs
	ExLeftRec x lrr -> case lrr of
		LrrInfix op ns xs -> fNmdApp (x:xs) op ns
		LrrPostfix xs op ns -> fNmdApp (x:xs) op ns
		LrrGrouping ns -> fNs ns $ self x
	ExCurriedLeft op ns xs -> fNmdApp (ExSlot:xs) op ns
	ExPrefix op ns xs -> fNmdApp xs op ns
	ExNamed x n -> fName (self x) n
	where
	-- aa = (,nAA)
	fName ast (Name ch l n r) = f2 $ f1 (f0 ast n)
		where
		f0 = case ch of
			'@' -> ABind
			'#' -> ALateBind
		[f1,f2] = map (maybe id (\f a->AExtract a f n)) [l,r]
			
	fNmdApp xs op ns = fNs ns $ AApplication (selfs xs) (fOp op)
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
data ASTAttachment
	= AA {}
	deriving (Eq,Read,Show,Ord,Typeable,Data)
nAA = AA {}
-- TODO: parameterize
-- type AST = (AST',ASTAttachment)
-- data AST'
type AST f = ASTExpr f
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

-----------------------------------------------------------------
-- Do not build explicit AST for now. 
-- use functions below to simulate the structure of ASTs
{-
namedApp::[Expr]->Operator->[Name]->([JStat], SymbolTable, JExpr)
namedApp xs op ns = foldl (application xs op) naming ns

application::[Expr]->Operator->([JStat], SymbolTable, JExpr)
application xs = \case
	OpSymbolic s
	OpAlphabetic s
	OpComposed '{' _ xs 
	OpComposed '[' _ xs
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
