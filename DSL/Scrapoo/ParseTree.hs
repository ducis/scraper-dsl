{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, 
	TypeOperators, TupleSections, LambdaCase, OverloadedStrings,
	NoMonomorphismRestriction, RelaxedPolyRec, ScopedTypeVariables #-}
module DSL.Scrapoo.ParseTree where

type Quote = String
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


